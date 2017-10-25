#!/usr/bin/perl -w
#
my $lockfilename="lock";
if ( not open(MLOCKFILE, ">$lockfilename") ) {
    die "Unable to open $lockfilename";
} elsif (flock (MLOCKFILE,2+4)) {
    system "touch start";
    sleep 5;
    system "touch stop";
} else {
    die "script already running.";
};
close(MLOCKFILE);

{
    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
	$mtime,$ctime,$blksize,$blocks) = stat("lock");
    print "Lock  $atime\n";
}
{
    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
	$mtime,$ctime,$blksize,$blocks) = stat("start");
    print "Start $atime\n";
}
{
    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
	$mtime,$ctime,$blksize,$blocks) = stat("stop");
    print "Stop  $atime\n";
}
