
            #include <sys/types.h>
            #include <dirent.h>
            int main()
            {
                DIR* d = opendir("/proc");
                if(d)
                    return 0;
                else
                    return -1;
            }
