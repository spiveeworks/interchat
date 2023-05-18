#ifndef INTERCHAT_WINDOW_H
#define INTERCHAT_WINDOW_H

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <errno.h>

#include "image.h"

enum window_event_type {
    WINDOW_NULL,
    WINDOW_RESIZE,
    WINDOW_KEY_DOWN,
    WINDOW_KEY_UP,
    WINDOW_BUTTON_DOWN,
    WINDOW_BUTTON_UP,
    WINDOW_MOUSE_MOVE,
    WINDOW_LOST_FOCUS,
    WINDOW_CLOSE,
};

enum window_keyboard_mask {
    WINDOW_MASK_NONE = 0x0,
    WINDOW_MASK_SHIFT = 0x1,
    WINDOW_MASK_CONTROL = 0x2,
};

struct window_event {
    enum window_event_type type;
    /* These fields could be uint8 and int16, but we want this file to be
       simple. */
    union {
        unsigned int key;
        unsigned char button;
    };
    enum window_keyboard_mask mask;
    short x;
    short y;
};

#ifdef _WIN32
  #include <windows.h>

  /* NOTE: This global is set by entrypoint.h, and so by simply reading it, we
     can magically work without any platform-specific code on the user side. We
     happen to declare it extern rather than explicitly include entrypoint.h,
     so that user code can define a new entrypoint without actually modifying
     this file, as long as they define and set the global themselves. If you
     find that solution ugly, then you can pick a better one, once there is an
     actual use case for window.h without entrypoint.h, but until then, they
     are designed to complement each other, so enjoy the magic. :) */
  extern HINSTANCE win32_application_instance;

  struct win32_handles {
      HWND w32_win;

      HBITMAP back_buffer_handle;
      HDC back_buffer_context;

      struct window_event event;
      struct window_event *event_overflow;
      int event_count;
      int events_processed;
  };
  typedef struct win32_handles platform_handles;
#else /* End of Windows, start of X11. */
  #include <X11/Xlib.h>
  #include <X11/Xutil.h>
  #include <X11/Xos.h>

  #include <sys/ipc.h>
  #include <sys/shm.h>
  #include <X11/extensions/XShm.h>

  struct x11_handles {
      Display *xdis;
      Window xwin;
      GC xgc;

      /* TODO: Create a platform agnostic image view that points to this. */
      XImage *back_buf;
      XShmSegmentInfo shm_info;
  };
  typedef struct x11_handles platform_handles;
#endif /* End of X11. */

struct window {
    platform_handles platform;

    int width;
    int height;

    int mouse_x;
    int mouse_y;

    bool recreate_back_buf;
    bool waiting_for_shm_completion;

    bool create_quarter_buf;
    bool back_buffer_drawn;

    ImageView back_buf;
    Image quarter_buf;

    bool key_down[256];
    bool button_down[7];
};

void create_window(
    struct window *win,
    char *window_name,
    bool create_quarter_buf
);
void create_back_buf(struct window *win);
bool get_event(struct window *win, struct window_event *event, bool block);
void flush_window(struct window *win);

void window_update_input_state(struct window *win, struct window_event *event) {
    switch (event->type) {
    case WINDOW_KEY_DOWN:
        if (event->key < 256) win->key_down[event->key] = true;
        break;
    case WINDOW_KEY_UP:
        if (event->key < 256) win->key_down[event->key] = false;
        break;
    case WINDOW_BUTTON_DOWN:
        if (event->button < 7) win->button_down[event->button] = true;
        break;
    case WINDOW_BUTTON_UP:
        if (event->button < 7) win->button_down[event->button] = false;
        break;
    case WINDOW_MOUSE_MOVE:
        win->mouse_x = event->x;
        win->mouse_y = event->y;
        break;
    case WINDOW_LOST_FOCUS:
        for (int i = 0; i < 256; i++) win->key_down[i] = false;
        for (int i = 0; i < 7; i++) win->button_down[i] = false;
        break;
    }
}

void create_back_and_quarter_buf(struct window *win) {
    if (win->quarter_buf.data) {
        free(win->quarter_buf.data);
        win->quarter_buf.data = NULL;
        win->quarter_buf.width = 0;
        win->quarter_buf.height = 0;
    }

    create_back_buf(win);

    if (win->create_quarter_buf) {
        /* If we are drawing 1:1 then we just use the back_buf, so worst
           case scenario is 2:1 with the whole screen shifted by half a
           pixel. */
        int half_width = win->width / 2 + 1;
        int half_height = win->height / 2 + 1;
        win->quarter_buf.data = malloc(4*half_width*half_height);
        win->quarter_buf.width = half_width;
        win->quarter_buf.height = half_height;
    }
}

#ifdef _WIN32 /* Start Windows implementations. */

LRESULT handle_win_event(
    HWND window,
    UINT message,
    WPARAM arg_w,
    LPARAM arg_l
);

void create_window(
    struct window *win,
    char *window_name,
    bool create_quarter_buf
) {
    *win = (struct window){0};
    struct win32_handles *w32 = &win->platform;

    WNDCLASSA w = {0};
    w.style = CS_OWNDC|CS_HREDRAW|CS_VREDRAW;
    w.lpfnWndProc = handle_win_event;
    w.hInstance = win32_application_instance;
    //w.hIcon = 0;
    w.lpszClassName = "Settlement Window Class";

    if (RegisterClass(&w) == 0) {
        OutputDebugStringA("Could not register window class.\n");
        exit(EXIT_FAILURE);
    }
    w32->w32_win = CreateWindowEx(
        0,
        w.lpszClassName,
        window_name,
        WS_OVERLAPPEDWINDOW|WS_VISIBLE,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        0,
        0,
        win32_application_instance,
        0
    );
    if (!w32->w32_win) {
        OutputDebugStringA("Could not create window.\n");
        exit(EXIT_FAILURE);
    }
    /* Store win, so that we can use it during event handling. */
    SetWindowLongPtrA(w32->w32_win, GWLP_USERDATA, (LONG_PTR)win);

    win->create_quarter_buf = create_quarter_buf;
    /* Create a back buffer immediately, since we probably got a WM_SIZE event
       that we couldn't do anything about when we created the window. */
    create_back_and_quarter_buf(win);
}

void win32_enqueue_abstract_event(
    struct win32_handles *w32,
    struct window_event *event
) {
    w32->event_count += 1;
    if (w32->event_count == 1) {
        w32->event = *event;
    } else if (w32->event_count == 2) {
        w32->event_overflow = malloc(sizeof(*event));
        w32->event_overflow[0] = *event;
    } else {
        w32->event_overflow = realloc(
            w32->event_overflow,
            w32->event_count * sizeof(*event)
        );
        w32->event_overflow[w32->event_count - 2] = *event;
    }
}

char convert_key(WPARAM arg_w) {
    if (arg_w >= '0' & arg_w <= '9') return arg_w;
    if (arg_w >= 'A' & arg_w <= 'Z') return 'a' + (arg_w - 'A');
    /* What do we do with numpad keys? + and - we combine with their non-numpad
       variants, for now. This whole apporach is probably quite different to
       what we actually want, in the long term. */
    /* else */
    switch (arg_w) {
    case VK_TAB:
        return '\t';
    case VK_ESCAPE:
        return 27; /* On GCC this would be '\e'. */
    case ' ':
        return ' ';
    case VK_OEM_1:
        return ';';
    case VK_ADD:
    case VK_OEM_PLUS:
        return '=';
    case VK_OEM_COMMA:
        return ',';
    case VK_SUBTRACT:
    case VK_OEM_MINUS:
        return '-';
    case VK_OEM_PERIOD:
        return '.';
    case VK_OEM_2:
        return '/';
    case VK_OEM_3:
        return '`';
    case VK_OEM_4:
        return '[';
    case VK_OEM_5:
    case VK_OEM_102:
        return '\\';
    case VK_OEM_6:
        return ']';
    case VK_OEM_7:
        return '\'';
    default:
        return 0;
    }
}

LRESULT handle_win_event(
    HWND window,
    UINT message,
    WPARAM arg_w,
    LPARAM arg_l
) {
    struct window *win =
        (struct window *)GetWindowLongPtrA(window, GWLP_USERDATA);
    struct win32_handles *w32 = &win->platform;

    struct window_event event = {0};

    switch(message) {
    case WM_KEYDOWN:
        event.key = convert_key(arg_w);
        if (event.key != 0) event.type = WINDOW_KEY_DOWN;
        break;
    case WM_KEYUP:
        event.key = convert_key(arg_w);
        if (event.key != 0) event.type = WINDOW_KEY_UP;
        break;
    case WM_MOUSEMOVE:
        event.type = WINDOW_MOUSE_MOVE;
        event.x = LOWORD(arg_l);
        event.y = HIWORD(arg_l);
        break;
    case WM_LBUTTONDOWN:
        /* TODO: Let's stop following the X event layout so strictly. */
        event.type = WINDOW_BUTTON_DOWN;
        event.button = 1;
        event.x = LOWORD(arg_l);
        event.y = HIWORD(arg_l);
        break;
    case WM_LBUTTONUP:
        event.type = WINDOW_BUTTON_UP;
        event.button = 1;
        event.x = LOWORD(arg_l);
        event.y = HIWORD(arg_l);
        break;
    case WM_MBUTTONDOWN:
        event.type = WINDOW_BUTTON_DOWN;
        event.button = 2;
        event.x = LOWORD(arg_l);
        event.y = HIWORD(arg_l);
        break;
    case WM_MBUTTONUP:
        event.type = WINDOW_BUTTON_UP;
        event.button = 2;
        event.x = LOWORD(arg_l);
        event.y = HIWORD(arg_l);
        break;
    case WM_RBUTTONDOWN:
        event.type = WINDOW_BUTTON_DOWN;
        event.button = 3;
        event.x = LOWORD(arg_l);
        event.y = HIWORD(arg_l);
        break;
    case WM_RBUTTONUP:
        event.type = WINDOW_BUTTON_UP;
        event.button = 3;
        event.x = LOWORD(arg_l);
        event.y = HIWORD(arg_l);
        break;
    case WM_MOUSEWHEEL:
        /* TODO: Where are the button up events? It seems like this peculiarity
           of X is something we want to avoid completely. */
        event.type = WINDOW_BUTTON_DOWN;
        if ((short)HIWORD(arg_w) > 0) event.button = 4;
        else event.button = 5;
        event.x = LOWORD(arg_l);
        event.y = HIWORD(arg_l);
        break;
    case WM_SIZE:
        if (win) {
            create_back_and_quarter_buf(win);
            event.type = WINDOW_RESIZE;
        }
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    case WM_CLOSE:
        PostQuitMessage(0);
        break;
    case WM_ACTIVATEAPP:
        break;
    case WM_PAINT:
      {
        PAINTSTRUCT paint;
        HDC ctx = BeginPaint(window, &paint);
        struct window *win =
            (struct window *)GetWindowLongPtrA(window, GWLP_USERDATA);

        if (win->back_buffer_drawn) {
            int x = paint.rcPaint.left;
            int y = paint.rcPaint.top;
            int width = paint.rcPaint.right - paint.rcPaint.left;
            int height = paint.rcPaint.bottom - paint.rcPaint.top;
            BOOL success = BitBlt(
                ctx,
                x, y,
                width, height,
                win->platform.back_buffer_context,
                x, y,
                SRCCOPY
            );
            if (success == 0) {
                int error = GetLastError();
                OutputDebugStringA("Warning: Could not Blit the back buffer.\n");
            }
        }

        EndPaint(window, &paint);
      }
        break;
    default:
        return DefWindowProc(window, message, arg_w, arg_l);
    }

    if (event.type != WINDOW_NULL) {
        win32_enqueue_abstract_event(&win->platform, &event);
    }

    return 0;
}

void create_back_buf(struct window *win) {
    struct win32_handles *w32 = &win->platform;

    if (w32->back_buffer_handle) {
        DeleteObject(w32->back_buffer_handle);
    }
    if (!w32->back_buffer_context) {
        w32->back_buffer_context = CreateCompatibleDC(0);
    }

    RECT client_rect;
    GetClientRect(w32->w32_win, &client_rect);
    win->width = client_rect.right - client_rect.left;
    win->height = client_rect.bottom - client_rect.top;

    BITMAPINFO info = {0};
    info.bmiHeader.biSize = sizeof(info.bmiHeader); /* May be unnecessary. */
    info.bmiHeader.biWidth = win->width;
    info.bmiHeader.biHeight = -win->height; /* negative for top down */
    info.bmiHeader.biPlanes = 1;
    info.bmiHeader.biBitCount = 32;
    info.bmiHeader.biCompression = BI_RGB;
    char *data;
    w32->back_buffer_handle = CreateDIBSection(
        w32->back_buffer_context,
        &info,
        DIB_RGB_COLORS,
        &data,
        0,
        0
    );

    SelectObject(w32->back_buffer_context, w32->back_buffer_handle);

    win->back_buf.data = data;
    win->back_buf.width = win->width;
    win->back_buf.height = win->height;
    win->back_buf.stride = 4*win->width;

    win->back_buffer_drawn = false;

    win->recreate_back_buf = false;
}

bool get_event(struct window *win, struct window_event *event, bool block) {
    struct win32_handles *w32 = &win->platform;

    bool queue_empty = false;
    while (true) {
        /* handle_win_event stores events in the window structure. We flush the
           one or so messages queued, and then look for WINAPI messages to
           dispatch, possibly causing handle_win_event to get called again, and
           so it continues. */
        if (w32->event_count > 0) {
            int i = w32->events_processed++;
            if (i < w32->event_count) {
                if (i == 0) *event = w32->event;
                else *event = w32->event_overflow[i - 1];
                window_update_input_state(win, event);
                return true;
            }
            /* else */
            if (w32->event_count > 1) free(w32->event_overflow);
            w32->event_count = 0;
            w32->events_processed = 0;
        }

        /* No events left in our internal queue, so look for messages to
           dispatch. */
        MSG msg;
        if (block) {
            BOOL result = GetMessage(&msg, 0, 0, 0);
            if (result < 0) {
                OutputDebugStringA("Warning: GetMessage failed. Quitting.\n");
                event->type = WINDOW_CLOSE;
                return true;
            }
        } else {
            BOOL got_message = PeekMessage(&msg, 0, 0, 0, PM_REMOVE);
            /* No messages in either queue, let the caller resume. */
            if (!got_message && w32->event_count == 0) return false;
        }
        if (msg.message == WM_QUIT) {
            event->type = WINDOW_CLOSE;
            return true;
        }
        /* else */

        /* We don't actually need WM_CHAR events yet. */
        /* TranslateMessage(&msg); */

        DispatchMessage(&msg);
    }
}

void flush_window(struct window *win) {
    HDC ctx = GetDC(win->platform.w32_win);
    BOOL success = BitBlt(
        ctx,
        0, 0,
        win->width, win->height,
        win->platform.back_buffer_context,
        0, 0,
        SRCCOPY
    );
    if (success == 0) {
        int error = GetLastError();
        OutputDebugStringA("Warning: Could not Blit the back buffer.\n");
    }
}

#else /* End of Windows implementations, start of X11 implementaions. */

void create_window(
    struct window *win,
    char *window_name,
    bool create_quarter_buf
) {
    *win = (struct window){0};
    struct x11_handles *x = &win->platform;

    Display *xdis = XOpenDisplay(NULL);
    x->xdis = xdis;

    Window xwin = XCreateSimpleWindow(
        xdis,
        DefaultRootWindow(xdis),
        0, 0,
        600, 600, 0, 0xFFFFFF, 0x000000);
    x->xwin = xwin;

    if (!XShmQueryExtension(xdis)) {
        fprintf(stderr, "Error: The shared memory extension is required for this program to run.\n");
        exit(EXIT_FAILURE);
    }

    XSetStandardProperties(xdis, xwin, window_name, window_name, None, NULL, 0, NULL);

    long mask = StructureNotifyMask | ButtonPressMask | ButtonReleaseMask
        | KeyPressMask | KeyReleaseMask | PointerMotionMask | FocusChangeMask
        | StructureNotifyMask;
    XSelectInput(xdis, xwin, mask);

    x->xgc = XCreateGC(xdis, xwin, 0, 0);

    XClearWindow(xdis, xwin);
    XMapRaised(xdis, xwin);

    win->width = 600;
    win->height = 600;

    win->create_quarter_buf = create_quarter_buf;
}

void create_back_buf(struct window *win) {
    struct x11_handles *x = &win->platform;
    if (x->back_buf) {
        XShmDetach(x->xdis, &x->shm_info);
        XDestroyImage(x->back_buf);
        shmdt(x->shm_info.shmaddr);
        shmctl(x->shm_info.shmid, IPC_RMID, 0);
    }
    unsigned depth;
    /* These are mostly unused, but we have to write them
       somewhere. */
    Window root;
    int offsetx;
    int offsety;
    unsigned border;

    XGetGeometry(x->xdis, x->xwin, &root, &offsetx, &offsety,
        &win->width, &win->height, &border, &depth);

    Visual *visual;
    {
        XVisualInfo attr;
        long mask = VisualClassMask | VisualDepthMask
            | VisualRedMaskMask | VisualGreenMaskMask
            | VisualBlueMaskMask;
        attr.class = TrueColor;
        attr.depth = 24;
        attr.red_mask = 0xFF0000;
        attr.green_mask = 0x00FF00;
        attr.blue_mask = 0x0000FF;

        int format_count;
        XVisualInfo *formats =
            XGetVisualInfo(x->xdis, mask, &attr, &format_count);
        if (format_count == 0) {
            fprintf(stderr, "Error: Currently only devices with 24-bit RGB colour are supported.\n");
            exit(EXIT_FAILURE);
        }

        visual = formats[0].visual;

        XFree(formats);
    }

    /* Seems to crash if we don't wait for shm completion between
       creating and recreating. Dunno why, but good to note. */
    XImage *ximg = XShmCreateImage(x->xdis, visual, 24, ZPixmap, NULL,
         &x->shm_info, win->width, win->height);
    x->back_buf = ximg;
    x->shm_info.shmid = shmget (IPC_PRIVATE,
        x->back_buf->bytes_per_line * x->back_buf->height, IPC_CREAT|0777);
    x->shm_info.shmaddr = x->back_buf->data = shmat(x->shm_info.shmid, 0, 0);
    x->shm_info.readOnly = true;
    XShmAttach(x->xdis, &x->shm_info);

    win->back_buf.data = ximg->data + ximg->xoffset;
    win->back_buf.width = ximg->width;
    win->back_buf.height = ximg->height;
    win->back_buf.stride = ximg->bytes_per_line;
    win->back_buffer_drawn = false;

    win->recreate_back_buf = false;
}

bool get_event(struct window *win, struct window_event *event_out, bool block) {
    struct x11_handles *x = &win->platform;
    const int SHM_COMPLETION = XShmGetEventBase (x->xdis) + ShmCompletion;

    struct window_event event = {0};
    while (true) {
        if (!block && (x->back_buf || win->recreate_back_buf)
            && !win->waiting_for_shm_completion
            && XEventsQueued(x->xdis, QueuedAlready) == 0)
        {
            if (win->recreate_back_buf) create_back_and_quarter_buf(win);
            return false;
        }

        XEvent xevent;
        XNextEvent(x->xdis, &xevent);

        if (xevent.type == ConfigureNotify) {
            XConfigureEvent *e = &xevent.xconfigure;
            if (!x->back_buf || e->width != win->width || e->height != win->height) {
                win->width = e->width;
                win->height = e->height;

                win->recreate_back_buf = true;
                /* App should know that a resize happened, so that it can
                   recalculate UI scale and redraw the window, although it
                   should wait until the back buffer is recreated! */
                event.type = WINDOW_RESIZE;
            }
        } else if (xevent.type == SHM_COMPLETION) {
            win->waiting_for_shm_completion = false;
        } else if (xevent.type == KeyPress) {
            KeySym key;
            XLookupString(&xevent.xkey, NULL, 0, &key, 0);

            event.type = WINDOW_KEY_DOWN;
            event.key = key;

            event.mask = WINDOW_MASK_NONE;
            if (xevent.xkey.state & ShiftMask) {
                event.mask |= WINDOW_MASK_SHIFT;
            }
            if (xevent.xkey.state & ControlMask) {
                event.mask |= WINDOW_MASK_CONTROL;
            }
        } else if (xevent.type == KeyRelease) {
            KeySym key;
            XLookupString(&xevent.xkey, NULL, 0, &key, 0);

            event.type = WINDOW_KEY_UP;
            event.key = key;
        } else if (xevent.type == ButtonPress) {
            event.type = WINDOW_BUTTON_DOWN;
            event.key = xevent.xbutton.button;
            event.x = xevent.xbutton.x;
            event.y = xevent.xbutton.y;
        } else if (xevent.type == ButtonRelease) {
            event.type = WINDOW_BUTTON_UP;
            event.key = xevent.xbutton.button;
            event.x = xevent.xbutton.x;
            event.y = xevent.xbutton.y;
        } else if (xevent.type == MotionNotify) {
            event.type = WINDOW_MOUSE_MOVE;
            event.x = xevent.xmotion.x;
            event.y = xevent.xmotion.y;
        } else if (xevent.type == FocusOut) {
            event.type = WINDOW_LOST_FOCUS;
        } else if (xevent.type == DestroyNotify) {
            event.type = WINDOW_CLOSE;
        }
        if (event.type != WINDOW_NULL) {
            window_update_input_state(win, &event);
            if (event_out) *event_out = event;
            return true;
        }
    }
}

void flush_window(struct window *win) {
    struct x11_handles *x = &win->platform;

    XEvent event;
    if (XCheckTypedEvent(x->xdis, DestroyNotify, &event)) {
        /* X lets us do this, so why not. We just want to get back to the event
           handler without breaking anything, to realise we need to exit. */
        XPutBackEvent(x->xdis, &event);
    } else {
        XShmPutImage(x->xdis, x->xwin, x->xgc, x->back_buf,
            0, 0, 0, 0, win->width, win->height, true);
        win->waiting_for_shm_completion = true;

        XFlush(x->xdis);
    }
}

#endif /* End of X11 implementations. */

#endif /* INTERCHAT_WINDOW_H */
