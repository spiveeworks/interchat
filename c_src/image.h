#ifndef INTERCHAT_IMAGE_H
#define INTERCHAT_IMAGE_H

typedef struct {
    unsigned char r;
    unsigned char g;
    unsigned char b;
} RGB;

typedef struct {
    unsigned char b;
    unsigned char g;
    unsigned char r;
    /* a is alpha, or just padding. */
    unsigned char a;
} RGBA;

/* Allocated block of memory containing pixels. */
typedef struct {
    RGBA *data;
    int width;
    int height;
} Image;

/* Rectangular slice into an Image or similar. Not to be confused with
   struct view in coords.h. */
typedef struct {
    unsigned char *data;
    int width;
    int height;
    int stride;
} ImageView;

#define VIEW_ENTIRE_IMAGE(I) ((ImageView){\
    (unsigned char*)(I).data,\
    (I).width,\
    (I).height,\
    4*(I).width\
})

ImageView view_entire_image(Image it) {
    return VIEW_ENTIRE_IMAGE(it);
}

#endif
