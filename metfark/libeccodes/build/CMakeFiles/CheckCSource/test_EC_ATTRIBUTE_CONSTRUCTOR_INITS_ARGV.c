
            #include <stdio.h>
            #include <string.h>
            int main(){return 0;}
            __attribute__ ((__constructor__))
            static void before_main(int argc, char* argv[], char* envp[])
            {
                printf("%d:%d",argc, strstr(argv[0],"cmTryCompileExec")?1:0);
            }
