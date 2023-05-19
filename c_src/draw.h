#ifndef INTERCHAT_DRAW_H
#define INTERCHAT_DRAW_H

#include <stdbool.h>
#include <stdint.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include "image.h"

/* Normally these types are defined in mem.h or util.h, but for now they are
   here, until I have a sense of what the project will need. */
typedef int8_t int8;
typedef int16_t int16;
typedef int32_t int32;
typedef int64_t int64;

typedef unsigned uint;
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;

typedef uint8_t byte;
typedef uint16_t word16;
typedef uint32_t word32;
typedef uint64_t word64;

typedef struct str {
    char *data;
    size_t size;
} str;

struct rectangle {
    int left;
    int top;
    int right;
    int bottom;
};

/* Freetype stuff */

FT_Library  library;
FT_Face     face;

#define GLYPH_COUNT 256
struct Glyph {
    int width;
    int height;
    int bearingx;
    int bearingy; /* ascent in XLib terms. */
    int advance;
    /* Glyphs have a single value/opacity channel, which we then blend onto the
     * screen ourselves. */
    byte *data;
};

struct Glyph glyphs[GLYPH_COUNT];
typedef struct Glyph *Glyph;

/* Our stuff */

typedef struct Sprite {
    ImageView image;
    int centre_x;
    int centre_y;
} Sprite;

#if 0
Image load_image(char *path) {
    Image image;

    image.data =
        (RGBA*)stbi_load(path, &image.width, &image.height, NULL, 4);

    if (!image.data) {
        if (path[0] == '/') {
            fprintf(stderr, "Failed to load file \"%s\".\n", path);
        } else {
            fprintf(stderr, "Failed to load file \"%s\", are you in the right "
                "directory?\n", path);
        }
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < image.width * image.height; i++) {
        byte r = image.data[i].r;
        image.data[i].r = image.data[i].b;
        image.data[i].b = r;
    }

    return image;
}
#endif

Image create_circle(byte r, byte g, byte b, int radius) {
    int diameter = 2 * radius;

    Image circle;
    circle.width = diameter;
    circle.height = diameter;
    circle.data = malloc(diameter * diameter * 4);

    for (int64 i = 0; i < diameter; i++) {
        for (int64 j = 0; j < diameter; j++) {
            int64 px = i * diameter + j;
            /* measure in half-pixels since we want the centre to be halfway
               between two pixels */
            int64 x = 2 * (i - radius) + 1;
            int64 y = 2 * (j - radius) + 1;
            int64 d = 2 * radius;
            if (x * x + y * y <= d * d) {
                circle.data[px] = (RGBA){.r = r, .g = g, .b = b, .a = 0xFF};
            } else {
                circle.data[px] = (RGBA){0x00, 0x00, 0x00, 0x00};
            }
        }
    }

    return circle;
}

void split_image(Image sheet, int columns, int rows, ImageView *out) {
    if (sheet.width % columns != 0) {
        fprintf(stderr, "A spritesheet was given with a width of %d, which is "
            "not divisible into %d columns.\n", sheet.width, columns);
        exit(EXIT_FAILURE);
    }
    if (sheet.height % rows != 0) {
        fprintf(stderr, "A spritesheet was given with a height of %d, which is"
            " not divisible into %d rows.\n", sheet.height, columns);
        exit(EXIT_FAILURE);
    }

    int tile_width = sheet.width / columns;
    int tile_height = sheet.height / rows;

    for (int j = 0; j < rows; j++) {
        int corner_y = j * tile_height;
        for (int i = 0; i < columns; i++) {
            int corner_x = i * tile_width;

            out->data =
                (byte*)(sheet.data + corner_y * sheet.width + corner_x);
            out->width = tile_width;
            out->height = tile_height;
            out->stride = 4 * sheet.width;
            out++;
        }
    }
}

void trim_sprite(Sprite *it) {
    int top = -1, left = -1, right = -1, bottom = -1;
    for (int y = 0; y < it->image.height; y++) {
        RGBA *row = (RGBA*)(it->image.data + y * it->image.stride);
        for (int x = 0; x < it->image.width; x++) {
            if (row[x].a > 0) {
                if (left == -1 || x < left) left = x;
                if (right == -1 || x > right) right = x;
                if (top == -1) top = y;
                bottom = y;
            }
        }
    }
    if (top == -1) {
        top = 0;
        left = 0;
        right = 0;
        bottom = 0;
    }

    it->image.width = right + 1;
    it->image.height = bottom + 1;

    it->image.data += top * it->image.stride;
    it->image.height -= top;
    it->centre_y -= top;
    it->image.data += 4 * left;
    it->image.width -= left;
    it->centre_x -= left;
}

Sprite create_sprite(Image image) {
    Sprite result;

    result.image = VIEW_ENTIRE_IMAGE(image);
    result.centre_x = image.width / 2;
    result.centre_y = image.height / 2;

    trim_sprite(&result);

    return result;
}

void create_sprites(Image sheet, int columns, int rows, Sprite *out) {
    int total = columns * rows;
    ImageView *views = alloca(total * sizeof(ImageView));

    split_image(sheet, columns, rows, views);

    for (int i = 0; i < total; i++) {
        out[i].image = views[i];
        out[i].centre_x = views[i].width / 2;
        out[i].centre_y = views[i].height / 2;

        trim_sprite(&out[i]);
    }
}

void init_text(void) {
    FT_Error error = FT_Init_FreeType(&library);
    if (error) {
        fprintf(stderr, "ERROR: failed to initialise FreeType library\n");
        exit(1);
    }

    error = FT_New_Face(
        library,
        "data/DejaVuSans.ttf",
         0,
         &face
    );
    if (error == FT_Err_Unknown_File_Format) {
        fprintf(stderr, "ERROR: unknown font format\n");
        exit(1);
    } else if (error) {
        fprintf(stderr, "ERROR: failed to load font\n");
        exit(1);
    }

    error = FT_Set_Pixel_Sizes(face, 0, 16);
    if (error) {
        fprintf(stderr, "ERROR: failed to set char size\n");
        exit(1);
    }

    for (int i = 0; i < GLYPH_COUNT; i++) {
        error = FT_Load_Char(face, i, 0);
        if (error)
            continue;  /* ignore errors */

        FT_Glyph_Metrics *metrics = &face->glyph->metrics;
        int width = metrics->width >> 6;
        int height = metrics->height >> 6;
        glyphs[i].width = width;
        glyphs[i].height = height;
        glyphs[i].width = metrics->width >> 6;
        glyphs[i].height = metrics->height >> 6;
        glyphs[i].bearingx = metrics->horiBearingX >> 6;
        glyphs[i].bearingy = metrics->horiBearingY >> 6;
        glyphs[i].advance = metrics->horiAdvance >> 6;

        glyphs[i].data = malloc(width * height);

        /* load glyph image into the slot (erase previous one) */
        error = FT_Load_Char(face, i, FT_LOAD_RENDER);
        if (error)
            continue;  /* ignore errors */

        /* save a copy of it */
        memcpy(glyphs[i].data, face->glyph->bitmap.buffer, width * height);
    }
}

void string_dimensions(
    str text,
    int *ascent_out,
    int *descent_out,
    int *width_out
) {
    int ascent = 0;
    int descent = 0;
    int width = 0;
    for (int i = 0; i < text.size; i++) {
        byte c = text.data[i];
        int this_ascent = glyphs[c].bearingy;
        int this_descent = glyphs[c].height - this_ascent;
        if (this_ascent > ascent) ascent = this_ascent;
        if (this_descent > descent) descent = this_descent;
        width += glyphs[c].advance;
    }
    if (ascent_out) *ascent_out = ascent;
    if (descent_out) *descent_out = descent;
    if (width_out) *width_out = width;
}

/*******************/
/* Basic Rendering */
/*******************/

void draw_sprite(ImageView *out, Sprite *sprite, int x, int y, int zoom) {
    int width = sprite->image.width;
    int height = sprite->image.height;

    /* X Clipping */
    int source_x = 0;
    int target_x = x - zoom * sprite->centre_x;
    if (target_x < 0) {
        int remove = -target_x;
        target_x = 0;
        source_x += remove;
        width -= remove;
    }
    if (target_x + width > out->width) {
        width -= target_x + width - out->width;
    }
    if (width <= 0) return;

    /* Y Clipping */
    int source_y = 0;
    int target_y = y - zoom * sprite->centre_y;
    if (target_y < 0) {
        int remove = -target_y;
        target_y = 0;
        source_y += remove;
        height -= remove;
    }
    if (target_y + height > out->height) {
        height -= target_y + height - out->height;
    }

    /* Image traversal stuff */
    int sprite_stride = sprite->image.stride;
    int out_stride = out->stride;
    byte *out_data = out->data;
    out_data += out_stride * target_y;
    out_data += target_x * 4;
    byte* source_row = sprite->image.data
        + source_y * sprite_stride
        + 4 * source_x;

    /* Time to draw! */
    for (int j = 0; j < height; j++) {
        /* Iterate source every few rows */
        word32 *source_start = (word32*)source_row;
        source_row += sprite_stride;
        word32 *source_end = source_start + width;

        for (int j_sub = 0; j_sub < zoom; j_sub++) {
            /* Iterate target every row */
            word32 *target = (word32*)out_data;
            out_data += out_stride;

            word32 *source = source_start;
            switch (zoom) {
            case 1:
                while (source < source_end) {
                    word32 pix = *source++;
                    if ((pix >> 24) > 0) {
                        target[0] = pix;
                    }
                    target += zoom;
                }
                break;
            case 2:
                while (source < source_end) {
                    word32 pix = *source++;
                    if ((pix >> 24) > 0) {
                        target[0] = pix;
                        target[1] = pix;
                    }
                    target += zoom;
                }
                break;
            case 3:
                while (source < source_end) {
                    word32 pix = *source++;
                    if ((pix >> 24) > 0) {
                        target[0] = pix;
                        target[1] = pix;
                        target[2] = pix;
                    }
                    target += zoom;
                }
                break;
            case 4:
                while (source < source_end) {
                    word32 pix = *source++;
                    if ((pix >> 24) > 0) {
                        target[0] = pix;
                        target[1] = pix;
                        target[2] = pix;
                        target[3] = pix;
                    }
                    target += zoom;
                }
                break;
            case 5:
                while (source < source_end) {
                    word32 pix = *source++;
                    if ((pix >> 24) > 0) {
                        target[0] = pix;
                        target[1] = pix;
                        target[2] = pix;
                        target[3] = pix;
                        target[4] = pix;
                    }
                    target += zoom;
                }
                break;
            default:
                while (source < source_end) {
                    word32 pix = *source++;
                    if ((pix >> 24) > 0) {
                        for (int k = 0; k < zoom; k++) target[k] = pix;
                    }
                    target += zoom;
                }
                break;
            }
        }
    }
}

void draw_string(ImageView *out, int x, int y, str text) {
    for (int ind = 0; ind < text.size; ind++) {
        uint8 c = text.data[ind];
        int width = glyphs[c].width;
        int height = glyphs[c].height;

        byte *out_data = out->data;
        int stride = out->stride;

        int corner_x = x + glyphs[c].bearingx;
        int corner_y = y - glyphs[c].bearingy;
        int start_i = corner_x < 0 ? -corner_x : 0;
        int start_j = corner_y < 0 ? -corner_y : 0;

        int end_i = corner_x + width > out->width ? out->width - corner_x : width;
        int end_j = corner_y + height > out->height ? out->height - corner_y : height;

        x += glyphs[c].advance;

        if (end_i <= start_i) continue;
        for (int j = start_j; j < end_j; j++) {
            byte *source_row = &glyphs[c].data[j * width];
            RGBA *target_row = (RGBA*)&out_data[stride * (corner_y + j)];
            target_row += corner_x;
            for (int i = start_i; i < end_i; i++) {
                uint16 a = source_row[i];
                a = a + 50 - (a + 50) % 51; /* round up to multiple of 255/5 */
                uint16 b = 255 - a;
                RGBA pix = target_row[i];
                pix.r = a + b * pix.r / 256;
                pix.g = a + b * pix.g / 256;
                pix.b = a + b * pix.b / 256;
                target_row[i] = pix;
            }
        }
    }
}

void draw_text_vformat(ImageView *out, int x, int y, char *fmt, va_list args) {
    char buf[128];
    size_t len = vsnprintf(buf, sizeof(buf), fmt, args);
    str text = {buf, len};
    char *buf2 = NULL;
    if (len > sizeof(buf)) {
        buf2 = malloc(len);
        vsnprintf(buf2, len, fmt, args);
        text.data = buf2;
    }
    int ascent, descent;
    string_dimensions(text, &ascent, &descent, NULL);
    draw_string(out, x, y, text);

    if (buf2) free(buf2);
}

void draw_text_format(ImageView *out, int x, int y, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    draw_text_vformat(out, x, y, fmt, args);

    va_end(args);
}

void fill_rectangle(ImageView *out, struct rectangle rect, RGBA colour_channels) {
    if (rect.left < 0) rect.left = 0;
    if (rect.right >= out->width) rect.right = out->width - 1;
    if (rect.top < 0) rect.top = 0;
    if (rect.bottom >= out->height) rect.bottom = out->height - 1;

    if (rect.left > rect.right) return;

    union {
        RGBA channels;
        word32 word;
    } colour;
    colour.channels = colour_channels;
    word32 word = colour.word;

    for (int j = rect.top; j <= rect.bottom; j++) {
        word32 *target_row = (word32*)&out->data[out->stride * j];
        for (int i = rect.left; i <= rect.right; i++) {
            target_row[i] = word;
        }
    }
}

void outline_rectangle(ImageView *out, struct rectangle rect, RGBA colour) {
    struct rectangle edge = rect;
    edge.bottom = rect.top;
    fill_rectangle(out, edge, colour);
    edge.top = edge.bottom = rect.bottom;
    fill_rectangle(out, edge, colour);
    edge = rect;
    edge.right = rect.left;
    fill_rectangle(out, edge, colour);
    edge.left = edge.right = rect.right;
    fill_rectangle(out, edge, colour);
}

#endif
