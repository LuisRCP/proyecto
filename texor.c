/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*** defines ***/

#define TEXOR_VERSION "0.0.1"
#define TEXOR_TAB_STOP 8
#define TEXOR_QUIT_TIMES 3

#define CTRL_KEY(k) ((k) & 0x1f)

enum editorKey {
  BACKSPACE = 127,
  ARROW_LEFT = 1000,
  ARROW_RIGHT,
  ARROW_UP,
  ARROW_DOWN,
  DEL_KEY,
  HOME_KEY,
  END_KEY,
  PAGE_UP,
  PAGE_DOWN
};

enum editorHighlight {
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*** data ***/

struct editorSyntax {
  char *file_type;
  char **file_match;
  char **keywords;
  char *singleline_comment_start;
  char *multiline_comment_start;
  char *multiline_comment_end;
  int flags;
};

typedef struct erow {
  int index;
  int size;
  int rendered_size;
  char *characters;
  char *rendered_characters;
  unsigned char *highlight;
  int highlight_open_comment;
} erow;

struct editorConfig {
  int file_position_x, file_position_y;
  int screen_position_x;
  int row_offset;
  int column_offset;
  int screen_rows;
  int screen_columns;
  int number_of_rows;
  erow *row;
  int dirty;
  char *filename;
  char status_message[80];
  time_t status_message_time;
  struct editorSyntax *syntax;
  struct termios orig_termios;
};

struct editorConfig E;

/*** filetypes ***/

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class", "case",

  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
  "void|", NULL
};

char *RUBY_HL_extensions[] = { ".rb", NULL };
char *RUBY_HL_keywords[] = {
  "__ENCODING__", "__LINE__", "__FILE__", "BEGIN", "END", "alias", "and",
  "begin", "break", "case", "class", "def", "defined?", "do", "else", "elsif",
  "end", "ensure", "false", "for", "if", "in", "module", "next", "nil", "not",
  "or", "redo", "rescue", "retry", "return", "self", "super", "then", "true",
  "undef", "unless", "until", "when", "while", "yield", NULL
};

struct editorSyntax HLDB[] = {
  {
    "c",
    C_HL_extensions,
    C_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
  {
    "ruby",
    RUBY_HL_extensions,
    RUBY_HL_keywords,
    "#", "=begin", "=end",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));
void texor();

/*** terminal ***/

void die(const char *s) {
  write(STDOUT_FILENO, "\x1b[2J", 4);
  write(STDOUT_FILENO, "\x1b[H", 3);
  perror(s);
  exit(1);
}

void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("tcsetattr");
}

void enableRawMode() {
  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
  atexit(disableRawMode);

  struct termios raw = E.orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

int editorReadKey() {
  int nread;
  char c;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN) die("read");
  }

  if (c == '\x1b') {
    char seq[3];

    if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
    if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

    if (seq[0] == '[') {
      if (seq[1] >= '0' && seq[1] <= '9') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
        if (seq[2] == '~') {
          switch (seq[1]) {
            case '1': return HOME_KEY;
            case '3': return DEL_KEY;
            case '4': return END_KEY;
            case '5': return PAGE_UP;
            case '6': return PAGE_DOWN;
            case '7': return HOME_KEY;
            case '8': return END_KEY;
          }
        }
      } else {
        switch (seq[1]) {
          case 'A': return ARROW_UP;
          case 'B': return ARROW_DOWN;
          case 'C': return ARROW_RIGHT;
          case 'D': return ARROW_LEFT;
          case 'H': return HOME_KEY;
          case 'F': return END_KEY;
        }
      }
    } else if (seq[0] == '0') {
      switch (seq[1]) {
        case 'H': return HOME_KEY;
        case 'F': return END_KEY;
      }
    }

    return '\x1b';
  } else {
    return c;
  }
}

int getCursorPosition(int *rows, int *cols) {
  char buf[32];
  unsigned int i = 0;

  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

  while (i < sizeof(buf) - 1) {
    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
    if (buf[i] == 'R') break;
    i++;
  }
  buf[i] = '\0';

  if (buf[0] != '\x1b' || buf[1] != '[') return -1;
  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

  return 0;
}

int getWindowSize(int *rows, int *cols) {
  struct winsize ws;

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
    return getCursorPosition(rows, cols);
  } else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

/*** syntax highlighting ***/

int is_separator(int c) {
  return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
  row->highlight = realloc(row->highlight, row->rendered_size);
  memset(row->highlight, HL_NORMAL, row->rendered_size);

  if (E.syntax == NULL) return;

  char **keywords = E.syntax->keywords;

  char *singleline_comment_start = E.syntax->singleline_comment_start;
  char *multiline_comment_start = E.syntax->multiline_comment_start;
  char *multiline_comment_end = E.syntax->multiline_comment_end;

  int singleline_comment_start_length = singleline_comment_start ? strlen(singleline_comment_start) : 0;
  int multiline_comment_start_length = multiline_comment_start ? strlen(multiline_comment_start) : 0;
  int multiline_comment_end_length = multiline_comment_end ? strlen(multiline_comment_end) : 0;

  int previous_separator = 1;
  int in_string = 0;
  int in_comment = (row->index > 0 && E.row[row->index - 1].highlight_open_comment);

  int i = 0;
  while (i < row->rendered_size) {
    char c = row->rendered_characters[i];
    unsigned char previous_highlight = (i > 0) ? row->highlight[i - 1] : HL_NORMAL;

    if (singleline_comment_start_length && !in_string && !in_comment) {
      if (!strncmp(&row->rendered_characters[i], singleline_comment_start, singleline_comment_start_length)) {
        memset(&row->highlight[i], HL_COMMENT, row->rendered_size - i);
        break;
      }
    }

    if (multiline_comment_start_length && multiline_comment_end_length && !in_string) {
      if (in_comment) {
        row->highlight[i] = HL_MLCOMMENT;
        if (!strncmp(&row->rendered_characters[i], multiline_comment_end, multiline_comment_end_length)) {
          memset(&row->highlight[i], HL_MLCOMMENT, multiline_comment_end_length);
          i += multiline_comment_end_length;
          in_comment = 0;
          previous_separator = 1;
          continue;
        } else {
          i++;
          continue;
        }
      } else if (!strncmp(&row->rendered_characters[i], multiline_comment_start, multiline_comment_start_length)) {
        memset(&row->highlight[i], HL_MLCOMMENT, multiline_comment_start_length);
        i += multiline_comment_start_length;
        in_comment = 1;
        continue;
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
      if (in_string) {
        row->highlight[i] = HL_STRING;
        if (c == '\\' && i + 1 < row->rendered_size) {
          row->highlight[i + 1] = HL_STRING;
          i += 2;
          continue;
        }
        if (c == in_string) in_string = 0;
        i++;
        previous_separator = 1;
        continue;
      } else {
        if (c == '"' || c == '\'') {
          in_string = c;
          row->highlight[i] = HL_STRING;
          i++;
          continue;
        }
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
      if ((isdigit(c) && (previous_separator || previous_highlight == HL_NUMBER)) ||
          (c == '.' && previous_highlight == HL_NUMBER)) {
        row->highlight[i] = HL_NUMBER;
        i++;
        previous_separator = 0;
        continue;
      }
    }

    if (previous_separator) {
      int j;
      for (j = 0; keywords[j]; j++) {
        int keyword_length = strlen(keywords[j]);
        int keyword2 = keywords[j][keyword_length - 1] == '|';
        if (keyword2) keyword_length--;

        if (!strncmp(&row->rendered_characters[i], keywords[j], keyword_length) &&
            is_separator(row->rendered_characters[i + keyword_length])) {
          memset(&row->highlight[i], keyword2 ? HL_KEYWORD2 : HL_KEYWORD1, keyword_length);
          i += keyword_length;
          break;
        }
      }
      if (keywords[j] != NULL) {
        previous_separator = 0;
        continue;
      }
    }

    previous_separator = is_separator(c);
    i++;
  }

  int changed = (row->highlight_open_comment != in_comment);
  row->highlight_open_comment = in_comment;
  if (changed && row->index + 1 < E.number_of_rows)
    editorUpdateSyntax(&E.row[row->index + 1]);
}

int editorSyntaxToColor(int highlight) {
  switch (highlight) {
    case HL_COMMENT:
    case HL_MLCOMMENT: return 36;
    case HL_KEYWORD1: return 33;
    case HL_KEYWORD2: return 32;
    case HL_STRING: return 35;
    case HL_NUMBER: return 31;
    case HL_MATCH: return 34;
    default: return 37;
  }
}

void editorSelectSyntaxHighlight() {
  E.syntax = NULL;
  if (E.filename == NULL) return;

  char *extension = strrchr(E.filename, '.');

  for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
    struct editorSyntax *s = &HLDB[j];
    unsigned int i = 0;
    while (s->file_match[i]) {
      int is_extension = (s->file_match[i][0] == '.');
      if ((is_extension && extension && !strcmp(extension, s->file_match[i])) ||
          (!is_extension && strstr(E.filename, s->file_match[i]))) {
        E.syntax = s;

        int file_row;
        for (file_row = 0; file_row < E.number_of_rows; file_row++) {
          editorUpdateSyntax(&E.row[file_row]);
        }

        return;
      }
      i++;
    }
  }
}

/*** row operations ***/

int editorRowCxToRx(erow *row, int cx) {
  int rx = 0;
  int j;
  for (j = 0; j < cx; j++) {
    if (row->characters[j] == '\t')
      rx += (TEXOR_TAB_STOP - 1) - (rx % TEXOR_TAB_STOP);
    rx++;
  }
  return rx;
}

int editorRowRxToCx(erow *row, int rx) {
  int current_rx = 0;
  int cx;
  for (cx = 0; cx < row->size; cx++) {
    if (row->characters[cx] == '\t')
      current_rx += (TEXOR_TAB_STOP - 1) - (current_rx % TEXOR_TAB_STOP);
    current_rx++;

    if (current_rx > rx) return cx;
  }
  return cx;
}

void editorUpdateRow(erow *row) {
  int tabs = 0;
  int j;
  for (j = 0; j < row->size; j++)
    if (row->characters[j] == '\t') tabs++;

  free(row->rendered_characters);
  row->rendered_characters = malloc(row->size + tabs*(TEXOR_TAB_STOP - 1) + 1);

  int idx = 0;
  for (j = 0; j < row->size; j++) {
    if (row->characters[j] == '\t') {
      row->rendered_characters[idx++] = ' ';
      while (idx % TEXOR_TAB_STOP != 0) row->rendered_characters[idx++] = ' ';
    } else {
      row->rendered_characters[idx++] = row->characters[j];
    }
  }
  row->rendered_characters[idx] = '\0';
  row->rendered_size = idx;

  editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t length) {
  if (at < 0 || at > E.number_of_rows) return;

  E.row = realloc(E.row, sizeof(erow) * (E.number_of_rows + 1));
  memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.number_of_rows - at));
  for (int j = at + 1; j <= E.number_of_rows; j++) E.row[j].index++;

  E.row[at].index = at;

  E.row[at].size = length;
  E.row[at].characters = malloc(length + 1);
  memcpy(E.row[at].characters, s, length);
  E.row[at].characters[length] = '\0';

  E.row[at].rendered_size = 0;
  E.row[at].rendered_characters = NULL;
  E.row[at].highlight = NULL;
  E.row[at].highlight_open_comment = 0;
  editorUpdateRow(&E.row[at]);

  E.number_of_rows++;
  E.dirty++;
}

void editorFreeRow(erow *row) {
  free(row->rendered_characters);
  free(row->characters);
  free(row->highlight);
}

void editorDelRow(int at) {
  if (at < 0 || at >= E.number_of_rows) return;
  editorFreeRow(&E.row[at]);
  memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.number_of_rows - at - 1));
  for (int j = at; j < E.number_of_rows - 1; j++) E.row[j].index--;
  E.number_of_rows--;
  E.dirty++;
}

void editorRowInsertChar(erow *row, int at, int c) {
  if (at < 0 || at > row->size) at = row->size;
  row->characters = realloc(row->characters, row->size + 2);
  memmove(&row->characters[at + 1], &row->characters[at], row->size - at + 1);
  row->size++;
  row->characters[at] = c;
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t length) {
  row->characters = realloc(row->characters, row->size + length + 1);
  memcpy(&row->characters[row->size], s, length);
  row->size += length;
  row->characters[row->size] = '\0';
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowDelChar(erow *row, int at) {
  if (at < 0 || at >= row->size) return;
  memmove(&row->characters[at], &row->characters[at + 1], row->size - at);
  row->size--;
  editorUpdateRow(row);
  E.dirty++;
}

/*** editor operations ***/

void editorInsertChar(int c) {
  if (E.cursor_y == E.number_of_rows) {
    editorInsertRow(E.number_of_rows, "", 0);
  }
  editorRowInsertChar(&E.row[E.cursor_y], E.cursor_x, c);
  E.cursor_x++;
}

void editorInsertNewline() {
  if (E.cursor_x == 0) {
    editorInsertRow(E.cursor_y, "", 0);
  } else {
    erow *row = &E.row[E.cursor_y];
    editorInsertRow(E.cursor_y + 1, &row->characters[E.cursor_x], row->size - E.cursor_x);
    row = &E.row[E.cursor_y];
    row->size = E.cursor_x;
    row->characters[row->size] = '\0';
    editorUpdateRow(row);
  }
  E.cursor_y++;
  E.cursor_x = 0;
}

void editorDelChar() {
  if (E.cursor_y == E.number_of_rows) return;
  if (E.cursor_x == 0 && E.cursor_y == 0) return;

  erow *row = &E.row[E.cursor_y];
  if (E.cursor_x > 0) {
    editorRowDelChar(row, E.cursor_x - 1);
    E.cursor_x--;
  } else {
    E.cursor_x = E.row[E.cursor_y - 1].size;
    editorRowAppendString(&E.row[E.cursor_y - 1], row->characters, row->size);
    editorDelRow(E.cursor_y);
    E.cursor_y--;
  }
}

/*** file i/o ***/

char *editorRowsToString(int *buffer_length) {
  int total_length = 0;
  int j;
  for (j = 0; j < E.number_of_rows; j++)
    total_length += E.row[j].size + 1;
  *buffer_length = total_length;

  char *buffer = malloc(total_length);
  char *p = buffer;
  for (j = 0; j < E.number_of_rows; j++) {
    memcpy(p, E.row[j].characters, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }

  return buffer;
}

void editorOpen(char *filename) {
  free(E.filename);
  E.filename = strdup(filename);

  editorSelectSyntaxHighlight();

  FILE *fp = fopen(filename, "r");
  if (!fp) die("fopen");

  char *line = NULL;
  size_t line_capacity = 0;
  ssize_t line_length;
  while ((line_length = getline(&line, &line_capacity, fp)) != -1) {
    while (line_length > 0 && (line[line_length - 1] == '\n' ||
                               line[line_length - 1] == '\r'))
      line_length--;
    editorInsertRow(E.number_of_rows, line, line_length);
  }
  free(line);
  fclose(fp);
  E.dirty = 0;
}

void editorSave() {
  if (E.filename == NULL) return;

  int length;
  char *buffer = editorRowsToString(&length);

  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
  if (fd != -1) {
    if (ftruncate(fd, length) != -1) {
      if (write(fd, buffer, length) == length) {
        close(fd);
        free(buffer);
        E.dirty = 0;
        editorSetStatusMessage("%d bytes written to disk", length);
        return;
      }
    }
    close(fd);
  }

  free(buffer);
  editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/*** find ***/

void editorFindCallback(char *query, int key) {
  static int last_match = -1;
  static int direction = 1;

  static int saved_highlight_line;
  static char *saved_highlight = NULL;

  if (saved_highlight) {
    memcpy(E.row[saved_highlight_line].highlight, saved_highlight, E.row[saved_highlight_line].rendered_size);
    free(saved_highlight);
    saved_highlight = NULL;
  }

  if (key == '\r' || key == '\x1b') {
    last_match = -1;
    direction = 1;
    return;
  } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
    direction = 1;
  } else if (key == ARROW_LEFT || key == ARROW_UP) {
    direction = -1;
  } else {
    last_match = -1;
    direction = 1;
  }

  if (last_match == -1) direction = 1;
  int current = last_match;
  int i;
  for (i = 0; i < E.number_of_rows; i++) {
    current += direction;
    if (current == -1) current = E.number_of_rows - 1;
    else if (current == E.number_of_rows) current = 0;

    erow *row = &E.row[current];
    char *match = strstr(row->rendered_characters, query);
    if (match) {
      last_match = current;
      E.cursor_y = current;
      E.cursor_x = editorRowRxToCx(row, match - row->rendered_characters);
      E.row_offset = E.number_of_rows;

      saved_highlight_line = current;
      saved_highlight = malloc(row->rendered_size);
      memcpy(saved_highlight, row->highlight, row->rendered_size);
      memset(&row->highlight[match - row->rendered_characters], HL_MATCH, strlen(query));
      break;
    }
  }
}

void editorFind() {
  int saved_cursor_x = E.cursor_x;
  int saved_cursor_y = E.cursor_y;
  int saved_column_offset = E.column_offset;
  int saved_row_offset = E.row_offset;

  char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)", editorFindCallback);

  if (query) {
    free(query);
  } else {
    E.cursor_x = saved_cursor_x;
    E.cursor_y = saved_cursor_y;
    E.column_offset = saved_column_offset;
    E.row_offset = saved_row_offset;
  }
}

/*** append buffer ***/

struct abuf {
  char *b;
  int length;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int length) {
  char *new = realloc(ab->b, ab->length + length);

  if (new == NULL) return;
  memcpy(&new[ab->length], s, length);
  ab->b = new;
  ab->length += length;
}

void abFree(struct abuf *ab) {
  free(ab->b);
}

/*** output ***/

void editorScroll() {
  E.rendered_cursor_x = 0;
  if (E.cursor_y < E.number_of_rows) {
    E.rendered_cursor_x = editorRowCxToRx(&E.row[E.cursor_y], E.cursor_x);
  }

  if (E.cursor_y < E.row_offset) {
    E.row_offset = E.cursor_y;
  }
  if (E.cursor_y >= E.row_offset + E.screen_rows) {
    E.row_offset = E.cursor_y - E.screen_rows + 1;
  }
  if (E.rendered_cursor_x < E.column_offset) {
    E.column_offset = E.rendered_cursor_x;
  }
  if (E.rendered_cursor_x >= E.column_offset + E.screen_columns) {
    E.column_offset = E.rendered_cursor_x - E.screen_columns + 1;
  }
}

void editorDrawRows(struct abuf *ab) {
  int y;
  for (y = 0; y < E.screen_rows; y++) {
    int filerow = y + E.row_offset;
    if (filerow >= E.number_of_rows) {
      if (E.number_of_rows == 0 && y == E.screen_rows / 3) {
        char welcome[80];
        int welcome_length = snprintf(welcome, sizeof(welcome),
          "Texor editor -- version %s", TEXOR_VERSION);
        if (welcome_length > E.screen_columns) welcome_length = E.screen_columns;
        int padding = (E.screen_columns - welcome_length) / 2;
        if (padding) {
          abAppend(ab, "~", 1);
          padding--;
        }
        while (padding--) abAppend(ab, " ", 1);
        abAppend(ab, welcome, welcome_length);
      } else {
        abAppend(ab, "~", 1);
      }
    } else {
      int length = E.row[filerow].rendered_size - E.column_offset;
      if (length < 0) length = 0;
      if (length > E.screen_columns) length = E.screen_columns;
      char *c = &E.row[filerow].rendered_characters[E.column_offset];
      unsigned char *highlight = &E.row[filerow].highlight[E.column_offset];
      int current_color = -1;
      int j;
      for (j = 0; j < length; j++) {
        if (iscntrl(c[j])) {
          char sym = (c[j] <= 26) ? '@' + c[j] : '?';
          abAppend(ab, "\x1b[7m", 4);
          abAppend(ab, &sym, 1);
          abAppend(ab, "\x1b[m", 3);
          if (current_color != -1) {
            char buffer[16];
            int color_length = snprintf(buffer, sizeof(buffer), "\x1b[%dm", current_color);
            abAppend(ab, buffer, color_length);
          }
        } else if (highlight[j] == HL_NORMAL) {
          if (current_color != -1) {
            abAppend(ab, "\x1b[39m", 5);
            current_color = -1;
          }
          abAppend(ab, &c[j], 1);
        } else {
          int color = editorSyntaxToColor(highlight[j]);
          if (color != current_color) {
            current_color = color;
            char buffer[16];
            int color_length = snprintf(buffer, sizeof(buffer), "\x1b[%dm", color);
            abAppend(ab, buffer, color_length);
          }
          abAppend(ab, &c[j], 1);
        }
      }
      abAppend(ab, "\x1b[39m", 5);
    }

    abAppend(ab, "\x1b[K", 3);
    abAppend(ab, "\r\n", 2);
  }
}

void editorDrawStatusBar(struct abuf *ab) {
  abAppend(ab, "\x1b[7m", 4);
  char status[80], rstatus[80];
  int length = snprintf(status, sizeof(status), "%.20s - %d lines %s",
    E.filename ? E.filename : "[No Name]", E.number_of_rows,
    E.dirty ? "(modified)" : "");
  int right_length = snprintf(rstatus, sizeof(rstatus), "%d/%d",
    E.cursor_y + 1, E.number_of_rows);
  if (length > E.screen_columns) length = E.screen_columns;
  abAppend(ab, status, length);
  while (length < E.screen_columns) {
    if (E.screen_columns - length == right_length) {
      abAppend(ab, rstatus, right_length);
      break;
    } else {
      abAppend(ab, " ", 1);
      length++;
    }
  }
  abAppend(ab, "\x1b[m", 3);
  abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
  abAppend(ab, "\x1b[K", 3);
  int message_length = strlen(E.status_message);
  if (message_length > E.screen_columns) message_length = E.screen_columns;
  if (message_length && time(NULL) - E.status_message_time < 5)
    abAppend(ab, E.status_message, message_length);
}

void editorRefreshScreen() {
  editorScroll();

  struct abuf ab = ABUF_INIT;

  abAppend(&ab, "\x1b[?25l", 6);
  abAppend(&ab, "\x1b[H", 3);

  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);

  char buffer[32];
  snprintf(buffer, sizeof(buffer), "\x1b[%d;%dH", (E.cursor_y - E.row_offset) + 1, (E.rendered_cursor_x - E.column_offset) + 1);
  abAppend(&ab, buffer, strlen(buffer));

  abAppend(&ab, "\x1b[?25h", 6);

  write(STDOUT_FILENO, ab.b, ab.length);
  abFree(&ab);
}

void editorSetStatusMessage(const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  vsnprintf(E.status_message, sizeof(E.status_message), format, ap);
  va_end(ap);
  E.status_message_time = time(NULL);
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
  size_t buffer_size = 128;
  char *buffer = malloc(buffer_size);

  size_t buffer_length = 0;
  buffer[0] = '\0';

  while (1) {
    editorSetStatusMessage(prompt, buffer);
    editorRefreshScreen();

    int c = editorReadKey();
    if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
      if (buffer_length != 0) buffer[--buffer_length] = '\0';
    } else if (c == '\x1b') {
      editorSetStatusMessage("");
      if (callback) callback(buffer, c);
      free(buffer);
      return NULL;
    } else if (c == '\r') {
      if (buffer_length != 0) {
        editorSetStatusMessage("");
        if (callback) callback(buffer, c);
        return buffer;
      }
    } else if (!iscntrl(c) && c < 128) {
      if (buffer_length == buffer_size - 1) {
        buffer_size *= 2;
        buffer = realloc(buffer, buffer_size);
      }
      buffer[buffer_length++] = c;
      buffer[buffer_length] = '\0';
    }

    if (callback) callback(buffer, c);
  }
}

/*** init ***/

void initEditor() {
  E.cursor_x = 0;
  E.cursor_y = 0;
  E.rendered_cursor_x = 0;
  E.row_offset = 0;
  E.column_offset = 0;
  E.number_of_rows = 0;
  E.row = NULL;
  E.dirty = 0;
  E.filename = NULL;
  E.status_message[0] = '\0';
  E.status_message_time = 0;
  E.syntax = NULL;

  if (getWindowSize(&E.screen_rows, &E.screen_columns) == -1) die("getWindowSize");
  E.screen_rows -= 2;
}

int main(int argc, char *argv[]) {
  enableRawMode();
  initEditor();
  if (argc >= 2) {
    editorOpen(argv[1]);
  }

  editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

  while (1) {
    editorRefreshScreen();
    editorProcessKeypress();
  }
  return 0;
}
