#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

void mod_opensession_(int* mid, char* crc250, int* irc, int len1);
void mod_clearfilestack_(int* mid,char* varname, char* crc250,int* irc, int len1, int len2);
void mod_cleartargetstack_(int* mid,char* crc250,int* irc, int len1);
void mod_closesession_(int* mid, char*crc250, int* irc, int len1);
void mod_setindex_(int* mid, char* trgname, char* varname, char* crc250, int* irc, int len1, int len2, int len3);
void mod_setindexlimits_(int* mid, char* smin, char* smax, char* crc250, int* irc, int len1, int len2, int len3);
void mod_determinefileorder_(int* mid, char* crc250, int* irc, int len1);
void mod_loadcache_(int* mid,char* path,char* crc250,int* irc, int len1,int len2);
void mod_makecache_(int* mid,char* path,char* crc250,int* irc, int len1,int len2);
void mod_peekfile_(int* mid,int* maxrep,int* nrep,char* rep250,char* crc250,int* irc, int len1,int len2);
void mod_peekfilelen_(int* mid,int* maxrep,char* crc250,int* irc, int len1);
void mod_popfile_(int* mid,char* path250,char* crc250,int* irc, int len1,int len2);
void mod_pushfile_(int* mid,char* path,char* crc250,int* irc, int len1,int len2);
void mod_pushtarget_(int* mid,char* trg,char* var,char* min,char* max, char* crc250,int* irc, int len1,int len2,int len3,int len4,int len5);
void mod_setfilter_ (int* cid, char* flt,  char* crc250, int* irc, int len1, int len2);

void obs_opensession_(int* oid,char* crc250,int* irc, int len1);
void obs_clearfilestack_(int* oid,char* crc250,int* irc, int len1);
void obs_cleartargetstack_(int* oid,char* crc250,int* irc, int len1);
void obs_closesession_(int* oid,char* crc250,int* irc, int len1);
void obs_loadcache_(int* oid,char* path,char* crc250,int* irc, int len1,int len2);
void obs_makecache_(int* oid,char* path,char* crc250,int* irc, int len1,int len2);
void obs_peekfile_(int* oid,int* maxrep,int* nrep,char* rep250,char* crc250,int* irc,int len1,int len2);
void obs_peekfilelen_(int* oid,int* maxrep,char* crc250,int* irc, int len1);
void obs_popfile_(int* oid,char* path250,char* crc250,int* irc, int len1,int len2);
void obs_pushfile_(int* oid,char* path,char* crc250,int* irc, int len1,int len2);
void obs_pushtarget_(int* oid,char* trg,char* pos,char* descr,char* info,char* min,char* max, char* crc250,int* irc, int len1,int len2,int len3,int len4,int len5,int len6,int len7);
void obs_settablepath_(int* oid,char* path250,char* crc250,int* irc, int len1,int len2);
void obs_setindex_(int* oid,char* trg80,char* exp250, char* crc250,int* irc, int len1,int len2,int len3);
void obs_setindexlimits_(int* oid,char* s25,char* e25,char* crc250,int* irc, int len1,int len2,int len3);
void obs_setbufrtype_(int* oid,int* bufrType, int* subType, char* crc250,int* irc, int len1);
void obs_setfilter_ (int* cid, char* flt,  char* crc250, int* irc, int len1, int len2);

void col_opensession_(int* cid, char* crc250, int* irc, int len1);
void col_closesession_(int* cid, char* crc250, int* irc, int len1);
void col_expression_(char* exp250, char* crc250, int* irc, int len1, int len2);
void col_setmodcache_(int* cid, char* path250, char* crc250, int* irc, int len1, int len2);
void col_setobscache_(int* cid, char* path250, char* crc250, int* irc, int len1, int len2);
void col_pushdefault_(int* cid,char* crc250,int* irc, int len1);
void col_cleardefaultstack_(int* cid,char* crc250,int* irc, int len1);
void col_adddefault_(int* cid,char* trg,char* val, char* crc250,int* irc, int len1,int len2,int len3);
void col_pushmatch_(int* cid, char* modName, char* obsExpr, char* smin, char* smax, char* crc250, int* irc, int len1, int len2, int len3, int len4, int len5);
void col_makematchlist_(int* cid, int* mid, char* crc250, int* irc, int len1);
void col_clearmatchstack_(int* mid, char* crc250, int* irc, int len1, int len2);
void col_makexml_(int* cid, int* mid, int* oid, char* xml250, int* test,char* fill250, char*  crc250, int* irc, int len1, int len2, int len3);
void col_setxmlfile_(int* pid, char* fn250, char* crc250, int* irc, int len1, int len2);
void col_getxmlfile_(int* pid, char* fn250, char* crc250, int* irc, int len1, int len2);

void plo_opensession_(int* pid, char* crc250, int* irc, int len1);
void plo_closesession_(int* pid, char* crc250, int* irc, int len1);
void plo_maketable_(int* pid, int* cid, int* mid, int* oid, char* tab250, char* gra250,char* cat250,int* test,char* fill250,char*  crc250, int* irc, int len1, int len2, int len3, int len4, int len5);
void plo_settype_(int* pid, char* type250, char* crc250, int* irc, int len1, int len2);
void plo_clearattrstack_(int* pid, char* crc250, int* irc, int len1, int len2);
void plo_pushattr_(int* pid, char* name250, char* value250, char* crc250, int* irc, int len1, int len2);
void plo_clearsetstack_(int* pid, char* crc250, int* irc, int len1, int len2);
void plo_clearcolumn_(int* pid, char* crc250, int* irc, int len1);
void plo_pushcolumn_(int* pid, char* name80, char* exp250, char* crc250, int* irc, int len1, int len2, int len3);
void plo_pushset_(int* pid, int* cid, int* mid, int* oid, char* name250, char* legend250, char* crc250, int* irc, int len1, int len2, int len3);
void plo_settablefile_(int* pid, char* fn250, char* crc250, int* irc, int len1, int len2);
void plo_gettablefile_(int* pid, char* fn250, char* crc250, int* irc, int len1, int len2);
void plo_setgraphicsfile_(int* pid, char* fn250, char* crc250, int* irc, int len1, int len2);
void plo_getgraphicsfile_(int* pid, char* fn250, char* crc250, int* irc, int len1, int len2);
void plo_strepfiles_(int* pid, char* crc250, int* irc, int len1);
void plo_setdebug_(int* ideb);

MODULE = fark		PACKAGE = fark

#####################################################################################
# SESSION methods
#####################################################################################
#
#  "openModelSession" creates a new session
#   Return array:
#      (integer) new session id
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_openModelSession();
    INIT: 
      int  mid;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char),251);
      strcpy(crc250,"");
      mod_opensession_(&mid,crc250,&irc,250);
      if (irc == 0) {
         strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 3);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      PUSHs(sv_2mortal(newSViv(mid)));
      free(crc250);

#
#  "closeModelSession" closes a session
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_closeModelSession(int mid);
    INIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 251);
      mod_closesession_(&mid,crc250, &irc, 250);
      if (irc == 0) {
         strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);


#
#  "openObsSession" creates a new session
#   Return array:
#      (integer) new session id
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_openObsSession();
    INIT:
      int  sid;
      int  irc;
      char *crc250;
    PPCODE:
      sid=0;
      irc=0;
      crc250 = calloc(sizeof(char), 251);
      strcpy(crc250,"");
      obs_opensession_(&sid,crc250, &irc, 250);
      if (irc == 0) {
         strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 3);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      PUSHs(sv_2mortal(newSViv(sid)));
      free(crc250);

#
#  "closeObsSession" closes a session
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_closeObsSession(int sid);
    INIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 251);
      obs_closesession_(&sid,crc250, &irc, 250);
      if (irc == 0) {
         strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "openColocSession" creates a new session
#   Return array:
#      (integer) new session id
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_openColocSession();
    INIT: 
      int  cid;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char),251);
      strcpy(crc250,"");
      col_opensession_(&cid,crc250,&irc,250);
      if (irc == 0) {
         strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 3);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      PUSHs(sv_2mortal(newSViv(cid)));
      free(crc250);

#
#  "closeColocSession" closes a session
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_closeColocSession(int cid);
    INIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 251);
      col_closesession_(&cid,crc250, &irc, 250);
      if (irc == 0) {
         strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "openPlotSession" creates a new session
#   Return array:
#      (integer) new session id
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void xs_openPlotSession();
    INIT: 
      int  pid;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char),251);
      strcpy(crc250,"");
      plo_opensession_(&pid,crc250,&irc,250);
      if (irc == 0) {
         strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 3);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      PUSHs(sv_2mortal(newSViv(pid)));
      free(crc250);

#
#  "closePlotSession" closes a session
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_closePlotSession(int pid);
    INIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 251);
      plo_closesession_(&pid,crc250, &irc, 250);
      if (irc == 0) {
         strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "makeModelCache" writes the model cache to a file.
#      (string) path to cache file.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_makeModelCache(int sid, char *path);
    PREINIT:
      int  irc;
      char *path250;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(path250,path);
      mod_makecache_(&sid,path250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);

#
#  "makeObsCache" writes the observation cache to a file.
#      (string) path to cache file.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_makeObsCache(int sid, char *path);
    PREINIT:
      int  irc;
      char *path250;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(path250,path);
      obs_makecache_(&sid, path250,crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);

#
#  "loadModelCache" loads the model cache from a file.
#      (string) path to cache file.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_loadModelCache(int sid, char *path);
    PREINIT:
      int  irc;
      char *path250;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(path250,path);
      mod_loadcache_(&sid,path250,crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);

#
#  "setModelCache" sets the model cache from a file.
#      (string) path to cache file.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_setModelCache(int cid, char *path);
    PREINIT:
      int  irc;
      char *path250;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(path250,path);
      col_setmodcache_(&cid, path250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);

#
#  "loadObsCache" loads the observation cache from a file.
#      (string) path to cache file.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_loadObsCache(int sid, char *path);
    PREINIT:
      int  irc;
      char *path250;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(path250,path);
      obs_loadcache_(&sid,path250,crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);

#
#  "setObsCache" sets the observation cache from a file.
#      (string) path to cache file.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_setObsCache(int cid, char *path);
    PREINIT:
      int  irc;
      char *path250;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(path250,path);
      col_setobscache_(&cid, path250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);


################################# MODEL ################################

#
#  "clearModelFileStack" clears the file stack and sets the variable config file.
#      (string) path to model configuration file (optional).
#      (string) variable used for sorting files, default is "time" (optional).
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_clearModelFileStack(int sid, ...);
    PREINIT:
      int  irc;
      char *varname;
      char *crc250;
      char* vararg;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      varname = calloc(sizeof(char), 80);
      strcpy(crc250,"");
      if (items >= 2) {
         vararg= (char *)SvPV_nolen(ST(1));
         strcpy(varname,vararg);
      } else {
         strcpy(varname,"");
      }; 
      mod_clearfilestack_(&sid,varname, crc250, &irc, 80, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(varname);
      free(crc250);

#
#  "clearModelTargetStack" clears the target stack.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_clearModelTargetStack(int sid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      mod_cleartargetstack_(&sid,crc250, &irc, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "clearDefaultStack" clears the default stack
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_clearDefaultStack(int sid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      col_cleardefaultstack_(&sid,crc250, &irc, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#  "pushModelFile" adds model file to the model file stack.
#      (string) ... paths to model files.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#   Files pushed to the file stack must be from the same model.
#   An error is returned if the file type is unknown.
#

void
xs_pushModelFile(int sid, ...);
    PREINIT:
      int  irc;
      char *crc250;
      int i;
      char *path250;
      char* path;
      int irc2;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      for (i=1; i < items; i++) {
         path= (char *)SvPV_nolen(ST(i));
         strcpy(path250,path);
	 irc2=0;
         #printf("fark.xs Push: %d %s\n",i,path250); 
         mod_pushfile_(&sid, path250, crc250, &irc2, 250, 250);
         if(irc2 != 0) {
	    irc=irc2;
	    i=items;
         }
      }
      if (irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);

#
#  "peekModelFile" returns data on the last model file on the file stack.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#      (integer) number of reports
#      (string array) report strings
#

void
xs_peekModelFile(int sid);
    PREINIT:
      int maxrep;
      int nrep;
      char *rep250;
      int  irc;
      char *crc250;
      int ii;
      int lenr=250;
    PPCODE:
	irc=0;
	crc250 = calloc(sizeof(char), 251);
	mod_peekfilelen_(&sid,&maxrep,crc250,&irc,250);
        nrep=0;
        if (irc == 0) {
	  rep250 = calloc(sizeof(char), lenr*maxrep+1);
	  mod_peekfile_(&sid,&maxrep,&nrep,rep250,crc250,&irc,250,250);
	} else {
	  rep250 = calloc(sizeof(char), 1);
        }
	if (irc != 0 ) {
	  EXTEND(SP, 2);
	  PUSHs(sv_2mortal(newSViv(irc)));
	  PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
	} else if (nrep > 0) {
	  EXTEND(SP, 3+nrep);
	  PUSHs(sv_2mortal(newSViv(irc)));
	  PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
	  PUSHs(sv_2mortal(newSViv(nrep)));
	  for (ii=0; ii < nrep ; ii++ ) {
	    PUSHs(sv_2mortal(newSVpv(&rep250[ii*250],250)));
	  };
	} else {
          # break while loop
	};
	free(rep250);
        free(crc250);



#
#  "popModelFile" removes the last model file from the file stack.
#      (string) ... names of files to be popped.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_popModelFile(int sid, ...);
    INIT:
      int  irc;
      char *crc250;
      int i;
      char *path250;
      char* path;
      int irc2;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 251);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      for (i=1; i < items; i++) {
         path= (char *)SvPV_nolen(ST(i));
         strcpy(path250,path);
	 irc2=0;
         mod_popfile_(&sid, path250, crc250, &irc2, 250, 250);
         if(irc2 != 0) {
	    irc=irc2;
         }
      }
      if (irc == 0) {
         strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);
      free(path250);


#
#  "setModelIndex"  sets the model index
#      (string) targetName
#      (string) modelVariable
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_setModelIndex(int sid, char *trgname, char *varname);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      mod_setindex_(&sid,trgname,varname,crc250, &irc, 25, 25, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "setModelIndexLimits" sets model limits values
#      (string) min value
#      (string) max value
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_setModelIndexLimits(int mid, char* smin, char* smax);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
	irc=0;
	crc250 = calloc(sizeof(char), 251);
 	irc=0;
        mod_setindexlimits_(&mid, smin, smax, crc250, &irc, strlen(smin),strlen(smax), 250);
        #printf("fark.xs Setindexlimits here: %d %s\n",irc,crc250); 
	EXTEND(SP, 2);
	PUSHs(sv_2mortal(newSViv(irc)));
	PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
        #printf("fark.xs Setindexlimits there: %d %s\n",irc,crc250); 
	free(crc250);
        #printf("fark.xs Setindexlimits done: %d\n",irc); 



#
#  "pushModelTarget"  sets the model targets
#      (string) modelTargetName
#      (string) variable
#      (string) min
#      (string) max
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_pushModelTarget(int mid, char *modName, char *var, char *vmin, char *vmax);
    PREINIT:
      int  irc;
      char *crc250;
      char *m80;
      char *v80;
      char *l80;
      char *u80;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      m80 = calloc(sizeof(char), 80);
      v80 = calloc(sizeof(char), 80);
      l80 = calloc(sizeof(char), 80);
      u80 = calloc(sizeof(char), 80);
      strcpy(crc250,"");
      strcpy(m80,modName);
      strcpy(v80,var);
      strcpy(l80,vmin);
      strcpy(u80,vmax);
      mod_pushtarget_(&mid, m80, v80, l80, u80,crc250, &irc, 80,80,80,80,250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(m80);
      free(v80);
      free(l80);
      free(u80);
      free(crc250);



#
#  "addDefault"  sets the model targets
#      (string) modelTargetName
#      (string) value
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_addDefault(int cid, char *modName, char *val);
    PREINIT:
      int  irc;
      char *crc250;
      char *m80;
      char *v80;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      m80 = calloc(sizeof(char), 80);
      v80 = calloc(sizeof(char), 80);
      strcpy(crc250,"");
      strcpy(m80,modName);
      strcpy(v80,val);
      col_adddefault_(&cid, m80, v80, crc250, &irc, 80,80,250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(m80);
      free(v80);
      free(crc250);


#
#  "pushDefault"  pushes the added default values to the stack
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_pushDefault(int cid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      col_pushdefault_(&cid, crc250, &irc, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);


#
#  "clearMatchRuleStack" clear the match rule stack
#   Return array:
#      (string)  result
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_clearMatchRuleStack(int cid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      col_clearmatchstack_(&cid,crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);
#
#  "pushMatchRule"  sets the match rules connecting model and observation targets
#      (string) modelTargetName
#      (string) obsTarget expression
#      (string) min
#      (string) max
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_pushMatchRule(int cid, char *modName, char *obsExpr, char *vmin, char *vmax);
    PREINIT:
      int  irc;
      char *crc250;
      char *m80;
      char *o250;
      char *l80;
      char *u80;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      m80 = calloc(sizeof(char), 80);
      o250 = calloc(sizeof(char), 250);
      l80 = calloc(sizeof(char), 80);
      u80 = calloc(sizeof(char), 80);
      strcpy(crc250,"");
      strcpy(m80,modName);
      strcpy(o250,obsExpr);
      strcpy(l80,vmin);
      strcpy(u80,vmax);
      col_pushmatch_(&cid, m80, o250, l80, u80,crc250, &irc, 80,250,80,80,250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(m80);
      free(o250);
      free(l80);
      free(u80);
      free(crc250);

#
#  "makeMatchList"  make match list
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_makeMatchList(int cid, int mid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      col_makematchlist_(&cid, &mid, crc250, &irc, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);


################################# OBSERVATIONS ##########################
#
#  "setTablePath" clears the file stack and sets the variable config file.
#      (string) path to BUFR tables
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_setObsTablePath(int oid, char *path);
    PREINIT:
      int  irc;
      char *path250;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(path250,path);
      obs_settablepath_(&oid, path250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);


#
#  "clearObsFileStack" clears the file stack and sets the variable config file.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_clearObsFileStack(int oid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      obs_clearfilestack_(&oid,crc250, &irc, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#  "pushObsFile" adds obs file to the obs file stack.
#      (string) ... paths to obs files.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#   Files pushed to the file stack must be from the same obs.
#   An error is returned if the file type is unknown.
#

void
xs_pushObsFile(int oid, ...);
    PREINIT:
      int  irc;
      char *crc250;
      int i;
      char *path250;
      char* path;
      int irc2;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      for (i=1; i < items; i++) {
         path= (char *)SvPV_nolen(ST(i));
         strcpy(path250,path);
	 irc2=0;
         obs_pushfile_(&oid, path250, crc250, &irc2, 250, 250);
         if(irc2 != 0) {
	    irc=irc2;
	    i=items;
         }
      }
      if (irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);

#
#  "peekObsFile" returns data on the last obs file on the file stack.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#      (integer) number of reports
#      (string array) report strings
#

void
xs_peekObsFile(int oid);
    PREINIT:
      int maxrep;
      int nrep;
      char *rep250;
      int  irc;
      char *crc250;
      int ii;
      int lenr=250;
    PPCODE:
	irc=0;
	crc250 = calloc(sizeof(char), 251);
	obs_peekfilelen_(&oid,&maxrep,crc250,&irc,250);
        nrep=0;
        if (irc == 0) {
	  rep250 = calloc(sizeof(char), lenr*maxrep+1);
	  obs_peekfile_(&oid,&maxrep,&nrep,rep250,crc250,&irc,250,250);
	} else {
	  rep250 = calloc(sizeof(char),1);
        }
        if (irc != 0 ) {
	    EXTEND(SP, 2);
	    PUSHs(sv_2mortal(newSViv(irc)));
	    PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
	} else if (nrep > 0) {
	  EXTEND(SP, 3+nrep);
	  PUSHs(sv_2mortal(newSViv(irc)));
	  PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
	  PUSHs(sv_2mortal(newSViv(nrep)));
	  for (ii=0; ii < nrep ; ii++ ) {
	    PUSHs(sv_2mortal(newSVpv(&rep250[ii*250],250)));
	  };
	} else {
            # break while loop
	};
	free(rep250);
        free(crc250);



#
#  "popObsFile" removes the last obs file from the file stack.
#      (string) ... names of files to be popped.
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_popObsFile(int oid, ...);
    INIT:
      int  irc;
      char *crc250;
      int i;
      char *path250;
      char* path;
      int irc2;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 251);
      path250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      for (i=1; i < items; i++) {
         path= (char *)SvPV_nolen(ST(i));
         strcpy(path250,path);
	 irc2=0;
         obs_popfile_(&oid, path250, crc250, &irc2, 250, 250);
         if(irc2 != 0) {
	    irc=irc2;
         }
      }
      if (irc == 0) {
         strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);
      free(path250);

#
#  "setObsIndexLimits"  sets start and stop limits for index
#      (string) start index
#      (string) stop index
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_setObsIndexLimits(int sid, char *sindex, char *eindex);
    PREINIT:
      int  irc;
      char *s25;
      char *e25;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      s25 = calloc(sizeof(char), 25);
      e25 = calloc(sizeof(char), 25);
      strcpy(crc250,"");
      strcpy(s25,sindex);
      strcpy(e25,eindex);
      #printf("fark.xs Calling setINdexLimits: %s %s\n",s25,e25); 
      obs_setindexlimits_(&sid,s25, e25, crc250, &irc, 25, 25, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(s25);
      free(e25);
      free(crc250);


#
#  "clearObsTargetStack"  clears all observation targets
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_clearObsTargetStack(int sid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      obs_cleartargetstack_(&sid,crc250, &irc, 250);
      if (irc == 0) {
          strcpy(crc250,"No error detected.");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);


#
#  "pushObsTarget"  adds an observation target to the target stack
#      (string) target name
#      (string) position
#      (string) descr
#      (string) info
#      (string) min
#      (string) max
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_pushObsTarget(int sid, char *target, char *pos, char *descr, char *info, char *vmin, char *vmax);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      obs_pushtarget_(&sid,target,pos,descr,info,vmin,vmax,crc250,&irc,strlen(target),strlen(pos),strlen(descr),strlen(info),strlen(vmin),strlen(vmax),250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "setObsIndex"  sets observation index
#      (string) target name
#      (string) expression
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_setObsIndex(int sid, char *target, char *expr);
    PREINIT:
      int  irc;
      char *crc250;
      char *t80;
      char *e250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      t80 = calloc(sizeof(char), 80);
      e250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(t80,target);
      strcpy(e250,expr);
      obs_setindex_(&sid, t80, e250,crc250, &irc, 80,250,250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(t80);
      free(e250);
      free(crc250);


#
#  "setObsBufrType"  sets observation type
#      (string) target name
#      (string) expression
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_setObsBufrType(int sid, int bufrType, int subType);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      obs_setbufrtype_(&sid, &bufrType, &subType, crc250, &irc, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);


#
#  "expression" evaluates expression
#      (string) expression.
#   Return array:
#      (string)  result
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_expression(char *exp);
    PREINIT:
      char *exp250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      exp250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(exp250,exp);
      col_expression_(exp250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      #printf("fark.xs Result: %s\n",exp250); 
      EXTEND(SP, 3);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      PUSHs(sv_2mortal(newSVpv(exp250,strlen(exp250))));
      free(exp250);
      free(crc250);


#
#  "setModelFilter" defines filter
#      (string) filter.
#   Return array:
#      (string)  result
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_setModelFilter(int cid, char *filter);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      mod_setfilter_(&cid,filter, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);
#
#  "setObsFilter" defines observation filter
#      (string) filtler.
#   Return array:
#      (string)  result
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_setObsFilter(int cid, char *filter);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      obs_setfilter_(&cid,filter, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "setColocXMLFile" set table file
#      (string) file name.
#   Return array:
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_setColocXMLFile(int pid, char *fn);
    PREINIT:
      char *fn250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      fn250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(fn250,fn);
      col_setxmlfile_(&pid, fn250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(fn250);
      free(crc250);

#
#  "getColocXMLFile" set table file
#   Return array:
#      (integer) error return code (0=ok)
#      (string)  error return message
#      (string) file name.

void
xs_getColocXMLFile(int pid);
    PREINIT:
      char *fn250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      fn250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(fn250,"");
      col_getxmlfile_(&pid, fn250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 3);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      PUSHs(sv_2mortal(newSVpv(fn250,strlen(fn250))));
      free(fn250);
      free(crc250);

#
#  "makeColocXML" make colocation XML file
#      (string) table file pattern.
#      (integer) test flag (0=only check input)
#      (string) fill file
#   Return array:
#      (string)  name of table file
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_makeColocXML(int cid, int mid, int oid, char *xml, int test, char *fill);
    PREINIT:
      char *xml250;
      int  irc;
      char *crc250;
      char *fill250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      fill250 = calloc(sizeof(char), 250);
      xml250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
strcpy(fill250,fill);
      strcpy(xml250,xml);
      col_makexml_(&cid, &mid, &oid, xml250, &test, fill250,crc250, &irc, 250, 250,250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 3);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      PUSHs(sv_2mortal(newSVpv(xml250,strlen(xml250))));
      free(xml250);
      free(fill250);
      free(crc250);

#
#  "makePlotTable" make table file (and find graphics file name)
#      (string) table file pattern.
#      (string) graphics file pattern.
#      (integer) test flag (0=only check input)
#      (string) fill file
#   Return array:
#      (string)  name of table file
#      (string)  name of graphics file
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_makePlotTable(int pid, int cid, int mid, int oid, char *table, char *graph, char* cat, int test, char *fill);
    PREINIT:
      char *tab250;
      char *gra250;
      char *cat250;
      char *fill250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      tab250 = calloc(sizeof(char), 250);
      gra250 = calloc(sizeof(char), 250);
      cat250 = calloc(sizeof(char), 250);
      fill250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(tab250,table);
      strcpy(gra250,graph);
      strcpy(cat250,cat);
      strcpy(fill250,fill);
      plo_maketable_(&pid,&cid,&mid,&oid,tab250,gra250,cat250,&test,fill250,crc250,&irc,250,250,250,250,250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 4);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      PUSHs(sv_2mortal(newSVpv(tab250,strlen(tab250))));
      PUSHs(sv_2mortal(newSVpv(gra250,strlen(gra250))));
      free(tab250);
      free(gra250);
      free(cat250);
      free(fill250);
      free(crc250);

#
#  "setPlotType" defines output file type (r-script name)
#      (string) type.
#   Return array:
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_setPlotType(int pid, char *type);
    PREINIT:
      char *type250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      type250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(type250,type);
      plo_settype_(&pid, type250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(type250);
      free(crc250);

#
#  "setPlotTableFile" set table file
#      (string) file name.
#   Return array:
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_setPlotTableFile(int pid, char *fn);
    PREINIT:
      char *fn250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      fn250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(fn250,fn);
      plo_settablefile_(&pid, fn250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(fn250);
      free(crc250);

#
#  "getPlotTableFile" set table file
#   Return array:
#      (integer) error return code (0=ok)
#      (string)  error return message
#      (string) file name.

void
xs_getPlotTableFile(int pid);
    PREINIT:
      char *fn250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      fn250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(fn250,"");
      plo_gettablefile_(&pid, fn250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 3);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      PUSHs(sv_2mortal(newSVpv(fn250,strlen(fn250))));
      free(fn250);
      free(crc250);

#
#  "setPlotGraphicsFile" set graphics file
#      (string) file name.
#   Return array:
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_setPlotGraphicsFile(int pid, char *fn);
    PREINIT:
      char *fn250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      fn250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(fn250,fn);
      plo_setgraphicsfile_(&pid, fn250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(fn250);
      free(crc250);

#
#  "getPlotGraphicsFile" set graphics file
#   Return array:
#      (integer) error return code (0=ok)
#      (string)  error return message
#      (string) file name.

void
xs_getPlotGraphicsFile(int pid);
    PREINIT:
      char *fn250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      fn250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(fn250,"");
      plo_getgraphicsfile_(&pid, fn250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 3);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      PUSHs(sv_2mortal(newSVpv(fn250,strlen(fn250))));
      free(fn250);
      free(crc250);

#
#  "strepPlotFiles" replace wildcards in files
#   Return array:
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_strepPlotFiles(int pid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      plo_strepfiles_(&pid, crc250, &irc, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "clearPlotAttributeStack" clear attribute stack.
#   Return array:
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_clearPlotAttributeStack(int pid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      plo_clearattrstack_(&pid, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "pushPlotAttribute" defines plot attribute
#      (string) name.
#      (string) value.
#   Return array:
#      (string)  result
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_pushPlotAttribute(int pid, char *name, char *value);
    PREINIT:
      char *name250;
      char *value250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      name250 = calloc(sizeof(char), 250);
      value250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(name250,name);
      strcpy(value250,value);
      plo_pushattr_(&pid, name250, value250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(name250);
      free(value250);
      free(crc250);

#
#  "clearPlotSetStack" clear set stack.
#   Return array:
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_clearPlotSetStack(int pid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      plo_clearsetstack_(&pid, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "clearPlotColumn" push plot column expression to the stack
#      (int)  plot session id
#   Return array:
#      (string)  result
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_clearPlotColumn(int pid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      plo_clearcolumn_(&pid, crc250, &irc, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "pushPlotColumn" push plot column expression to the stack
#      (int)  plot session id
#      (string)  name
#      (string)  expr
#   Return array:
#      (string)  result
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_pushPlotColumn(int pid, char *name, char *expr);
    PREINIT:
      char *name80;
      char *exp250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      name80 = calloc(sizeof(char), 80);
      exp250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(name80,name);
      strcpy(exp250,expr);
      plo_pushcolumn_(&pid, name80, exp250, crc250, &irc, 80, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(name80);
      free(exp250);
      free(crc250);

#
#  "pushPlotSet" push plot set to the stack
#      (int)  plot session id
#      (int)  coloc session id
#      (int)  model session id
#      (int)  obs session id
#      (string)  name
#      (string)  x
#      (string)  y
#      (string)  legend
#   Return array:
#      (string)  result
#      (integer) error return code (0=ok)
#      (string)  error return message

void
xs_pushPlotSet(int pid, int cid, int mid, int oid, char *name, char *legend);
    PREINIT:
      char *name80;
      char *legend250;
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      name80 = calloc(sizeof(char), 80);
      legend250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      strcpy(name80,name);
      strcpy(legend250,legend);
      plo_pushset_(&pid, &cid, &mid, &oid, name80, legend250, crc250, &irc, 80, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      };
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(name80);
      free(legend250);
      free(crc250);
#
#  "setDebug" set debug flag
#      (int)  1=on, 0=off

void
xs_setDebug(int ideb);
    PPCODE:
      plo_setdebug_(&ideb);
