#ifndef INTERCHAT_ENTRYPOINT
#define INTERCHAT_ENTRYPOINT

int entrypoint(int argc, char **argv);

#ifdef _WIN32

#include <windows.h>

HINSTANCE win32_application_instance;

int APIENTRY WinMain(
    HINSTANCE instance,
    HINSTANCE instance_prev,
    PSTR cmdline,
    int cmdshow
) {
    win32_application_instance = instance;

    char *argblob = GetCommandLineA();
    int blob_size = strlen(argblob);

    char *argbuf = malloc(blob_size + 1);
    int argc = 0;
    /* Generously assume less than 16 arguments. This is 1KiB. */
    char **argv = malloc(16 * sizeof(char*));
    /* Just split words, who cares. */
    int i = 0;
    while (i < blob_size) {
        char *arg_start = argbuf;
        while (argblob[i] != ' ' && argblob[i] != '\0') {
            *argbuf++ = argblob[i];
            i++;
        }
        *argbuf++ = '\0';

        if (argc >= 16) argv = realloc(argv, (argc + 1)*sizeof(char*));
        argv[argc] = arg_start;
        argc++;

        while (argblob[i] == ' ') i++;
    }
    entrypoint(argc, argv);
}

#else /* End of Windows command line parsing. Default to main. */

int main(int argc, char **argv) {
    entrypoint(argc, argv);
}

#endif /* End of entry points. */

#endif /* INTERCHAT_ENTRYPOINT */
