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
  int binary_mode;  // Added flag to handle binary mode
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

// Note: Highlighting removed as per your request

void editorUpdateSyntax(erow *row) {
  row->highlight = realloc(row->highlight, row->rendered_size);
  memset(row->highlight, HL_NORMAL, row->rendered_size);

  if (E.syntax == NULL) return;

  char **keywords = E.syntax->keywords;

  char *singleline_comment_start = E.syntax->singleline_comment_start;
  char *multiline_comment_start = E.syntax->multiline_comment_start;
  char *multiline_comment_end = E.syntax->multiline_comment_end;
  int flags = E.syntax->flags;

  int i = 0;
  int j = 0;
  int in_comment = 0;

  while (i < row->rendered_size) {
    char c = row->rendered_characters[i];

    if (singleline_comment_start && !in_comment) {
      if (c == singleline_comment_start[0] && row->rendered_characters[i+1] == singleline_comment_start[1]) {
        memset(&row->highlight[i], HL_COMMENT, row->rendered_size - i);
        break;
      }
    }

    if (multiline_comment_start && !in_comment) {
      if (c == multiline_comment_start[0] && row->rendered_characters[i+1] == multiline_comment_start[1]) {
        in_comment = 1;
        row->highlight[i] = HL_COMMENT;
        i += 2;
        continue;
      }
    }

    if (multiline_comment_end && in_comment) {
      if (c == multiline_comment_end[0] && row->rendered_characters[i+1] == multiline_comment_end[1]) {
        in_comment = 0;
        row->highlight[i] = HL_COMMENT;
        i += 2;
        continue;
      } else {
        row->highlight[i] = HL_COMMENT;
      }
    }

    if (flags & HL_HIGHLIGHT_STRINGS) {
      if (c == '"') {
        for (j = i + 1; j < row->rendered_size; j++) {
          if (row->rendered_characters[j] == '"' && row->rendered_characters[j-1] != '\\') break;
        }
        memset(&row->highlight[i], HL_STRING, j - i + 1);
        i = j;
        continue;
      }
    }

    if (flags & HL_HIGHLIGHT_NUMBERS) {
      if (isdigit(c)) {
        for (j = i + 1; j < row->rendered_size; j++) {
          if (!isdigit(row->rendered_characters[j])) break;
        }
        memset(&row->highlight[i], HL_NUMBER, j - i);
        i = j - 1;
        continue;
      }
    }

    if (flags & HL_HIGHLIGHT_KEYWORDS) {
      for (j = 0; keywords[j]; j++) {
        int len = strlen(keywords[j]);
        if (strncmp(&row->rendered_characters[i], keywords[j], len) == 0 &&
            !isalpha(row->rendered_characters[i + len]) &&
            !isdigit(row->rendered_characters[i + len])) {
          memset(&row->highlight[i], HL_KEYWORD1, len);
          i += len - 1;
          break;
        }
      }
    }

    i++;
  }
}

void editorScroll() {
  // Removed: No highlighting, no search
}

/*** input ***/

void editorMoveCursor(int key) {
  // Removed: No highlight handling
}

void editorProcessKeypress() {
  static int quit_times = TEXOR_QUIT_TIMES;

  int c = editorReadKey();

  switch (c) {
    case CTRL_KEY('q'):
      if (E.dirty && quit_times > 0) {
        editorSetStatusMessage("WARNING!!! File has unsaved changes. Press Ctrl-Q %d more times to quit.", quit_times);
        quit_times--;
        return;
      }
      write(STDOUT_FILENO, "\x1b[2J", 4);
      write(STDOUT_FILENO, "\x1b[H", 3);
      exit(0);
      break;

    case CTRL_KEY('t'):
      E.binary_mode = !E.binary_mode;
      editorSetStatusMessage("Binary mode %s", E.binary_mode ? "ON" : "OFF");
      break;

    default:
      break;
  }
}

/*** output ***/

void editorSetStatusMessage(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.status_message, sizeof(E.status_message), fmt, ap);
  va_end(ap);
  E.status_message_time = time(NULL);
}

/*** initialization ***/

void initEditor() {
  E.file_position_x = 0;
  E.file_position_y = 0;
  E.screen_position_x = 0;
  E.row_offset = 0;
  E.column_offset = 0;
  E.screen_rows = 0;
  E.screen_columns = 0;
  E.number_of_rows = 0;
  E.row = NULL;
  E.dirty = 0;
  E.filename = NULL;
  E.status_message[0] = '\0';
  E.status_message_time = 0;
  E.syntax = NULL;
  E.binary_mode = 0;
}

/*** main ***/

int main(int argc, char *argv[]) {
  enableRawMode();
  initEditor();

  if (argc >= 2) {
    E.filename = argv[1];
  }

  while (1) {
    editorRefreshScreen();
    editorProcessKeypress();
  }

  return 0;
}
