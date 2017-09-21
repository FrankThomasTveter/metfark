use 5.014002;
#use lib '/elysium/franktt/mir/fark/src/libfark';
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'fark',
    VERSION_FROM      => 'lib/fark.pm', # finds $VERSION
    PREREQ_PM         => {}, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/fark.pm', # retrieve abstract from module
       AUTHOR         => 'Frank Thomas Tveter <franktt@>') : ()),
    LIBS              => ['-Llib -lobs -lmodel -lcoloc -lplot -lparse -lgenc -lbufr -lbufrc -lgfortran -L/usr/lib -lnetcdff'], # e.g., '-lm'
    ##LDDLFLAGS         => '-shared -O2 -g -fstack-protector -Wl,-rpath,/usr/lib/',
    DEFINE            => '', # e.g., '-DHAVE_SOMETHING'
    INC               => '-I. -Ilib', # e.g., '-I. -I/usr/include/other'
    # Un-comment this if you add C files to link with later:
    # OBJECT            => '$(O_FILES)', # link all the C files too
);
