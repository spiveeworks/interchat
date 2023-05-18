#include <stdbool.h>

#include "window.h"
#include "entrypoint.h"
#include "draw.h"

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

typedef char *char_buffer;
typedef char_buffer *string_buffer;

struct text_state {
    string_buffer lines;
    int64 cursor_blink_frame;
    int cursor_row;
    int cursor_column;
};

void draw_frame(int64 frame, struct window *win, struct text_state *text) {
    ImageView out = win->back_buf;
    char *row = out.data;
    for (int j = 0; j < out.height; j++) {
        unsigned *pel = (unsigned*)row;
        unsigned *end = pel + out.width;
        for (int i = 0; i < out.width; i++) {
            *pel = 0;
            pel++;
        }
        while (pel < end) *pel++ = 0x888888;
        row += out.stride;
    }

    for (int i = 0; i < arrlen(text->lines); i++) {
        char_buffer line_buff = text->lines[i];
        str line = {line_buff, arrlen(line_buff)};
        draw_string(&out, 5, 20 * i + 20, line);
    }

    if ((frame - text->cursor_blink_frame) / 240 % 2 == 0) {
        str selected_line = {text->lines[text->cursor_row], text->cursor_column};
        int text_width = 0;
        string_dimensions(selected_line, NULL, NULL, &text_width);

        int cursor_x = 5 + text_width;
        int cursor_y = 20 * text->cursor_row + 20;

        struct rectangle cursor = {cursor_x, cursor_y - 20, cursor_x, cursor_y};
        RGBA white = {255, 255, 255, 255};
        fill_rectangle(&out, cursor, white);
    }

    win->back_buffer_drawn = true;
}

int entrypoint(int argc, char **argv) {
    init_text();

    struct window win;
    create_window(&win, "InterChat Messaging Client", false);

    struct text_state text = {0};
    arrpush(text.lines, NULL);
    arrpush(text.lines[0], 'h');
    arrpush(text.lines[0], 'i');
    arrpush(text.lines, NULL);
    arrpush(text.lines[1], ':');
    arrpush(text.lines[1], ')');

    long frame = 0;
    while (true) {
        struct window_event event;
        if (get_event(&win, &event, false)) {
            if (event.type == WINDOW_CLOSE) {
                exit(EXIT_SUCCESS);
            }
            if (event.type == WINDOW_KEY_DOWN) {
                /* left */
                if (event.key == 65361) {
                    if (text.cursor_column > 0) {
                        text.cursor_column -= 1;
                        text.cursor_blink_frame = frame;
                    } else if (text.cursor_row > 0) {
                        text.cursor_row -= 1;
                        text.cursor_column = arrlen(text.lines[text.cursor_row]);
                        text.cursor_blink_frame = frame;
                    }
                }
                /* up */
                if (event.key == 65362 && text.cursor_row > 0) {
                    text.cursor_row -= 1;
                    text.cursor_blink_frame = frame;
                }
                /* right */
                if (event.key == 65363) {
                    if (text.cursor_column < arrlen(text.lines[text.cursor_row])) {
                        text.cursor_column += 1;
                        text.cursor_blink_frame = frame;
                    } else if (text.cursor_row < arrlen(text.lines) - 1) {
                        text.cursor_row += 1;
                        text.cursor_column = 0;
                        text.cursor_blink_frame = frame;
                    }
                }
                /* down */
                if (event.key == 65364 && text.cursor_row < arrlen(text.lines) - 1) {
                    text.cursor_row += 1;
                    text.cursor_blink_frame = frame;
                }
                /* del */
                if (event.key == 65535) {
                    if (text.cursor_column < arrlen(text.lines[text.cursor_row])) {
                        arrdel(text.lines[text.cursor_row], text.cursor_column);

                        text.cursor_blink_frame = frame;
                    } else if (text.cursor_row < arrlen(text.lines) - 1) {
                        char_buffer remove = text.lines[text.cursor_row + 1];
                        arrdel(text.lines, text.cursor_row + 1);
                        char *spot = arraddnptr(text.lines[text.cursor_row], arrlen(remove));
                        memcpy(spot, remove, arrlen(remove));
                        arrfree(remove);

                        text.cursor_blink_frame = frame;
                    }
                }
                /* backspace */
                if (event.key == 65288) {
                    if (text.cursor_column > 0) {
                        arrdel(text.lines[text.cursor_row], text.cursor_column - 1);

                        text.cursor_column -= 1;
                        text.cursor_blink_frame = frame;
                    } else if (text.cursor_row > 0) {
                        char_buffer remove = text.lines[text.cursor_row];
                        arrdel(text.lines, text.cursor_row);

                        text.cursor_row -= 1;
                        text.cursor_column = arrlen(text.lines[text.cursor_row]);

                        char *spot = arraddnptr(text.lines[text.cursor_row], arrlen(remove));
                        memcpy(spot, remove, arrlen(remove));
                        arrfree(remove);

                        text.cursor_blink_frame = frame;
                    }
                }
                /* enter */
                if (event.key == 65293) {
                    char_buffer sel = text.lines[text.cursor_row];
                    char *rest_data = &sel[text.cursor_column];
                    int rest_len = arrlen(sel) - text.cursor_column;

                    char_buffer new_line = NULL;
                    char *spot = arraddnptr(new_line, rest_len);
                    memcpy(spot, rest_data, rest_len);
                    arrsetlen(sel, text.cursor_column);

                    arrins(text.lines, text.cursor_row + 1, new_line);
                    text.cursor_row += 1;
                    text.cursor_column = 0;
                    text.cursor_blink_frame = frame;
                }

                if (event.key >= 32 && event.key <= 127) {
                    arrins(text.lines[text.cursor_row], text.cursor_column, (char)event.key);

                    text.cursor_column += 1;
                    text.cursor_blink_frame = frame;
                }
                //printf("key: %d\n", event.key);
            }
            continue;
        }

        if (arrlen(text.lines) == 0) arrpush(text.lines, NULL);

        if (text.cursor_row < 0) text.cursor_row = 0;
        if (text.cursor_row >= arrlen(text.lines)) text.cursor_row = arrlen(text.lines) - 1;

        char_buffer sel = text.lines[text.cursor_row];
        if (text.cursor_column < 0) text.cursor_column = 0;
        if (text.cursor_column > arrlen(sel)) text.cursor_column = arrlen(sel);

        draw_frame(frame, &win, &text);
        flush_window(&win);

        frame++;
    }

    return 0;
}
