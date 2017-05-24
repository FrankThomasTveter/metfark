#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

void mod_opensession_(int* mid, char* crc250, int* irc, int len1);
void mod_clearfilestack_(int* mid,char* varname, char* crc250,int* irc, int len1, int len2);
void mod_cleartargetstack_(int* mid,char* crc250,int* irc, int len1);
void mod_cleardefaultstack_(int* mid,char* crc250,int* irc, int len1);
void mod_closesession_(int* mid, char*crc250, int* irc, int len1);
void mod_setindex_(int* mid, char* varname, char* crc250, int* irc, int len1, int len2, int len3);
void mod_setindexlimits_(int* mid, char* smin, char* smax, char* crc250, int* irc, int len1, int len2, int len3);
void mod_determinefileorder_(int* mid, char* crc250, int* irc, int len1);
void mod_loadcache_(int* mid,char* path,char* crc250,int* irc, int len1,int len2);
void mod_makecache_(int* mid,char* path,char* crc250,int* irc, int len1,int len2);
void mod_peekfile_(int* mid,int* maxrep,int* nrep,char* rep250,char* crc250,int* irc, int len1,int len2);
void mod_peekfilelen_(int* mid,int* maxrep,char* crc250,int* irc, int len1);
void mod_popfile_(int* mid,char* path250,char* crc250,int* irc, int len1,int len2);
void mod_pushfile_(int* mid,char* path,char* crc250,int* irc, int len1,int len2);
void mod_pushtarget_(int* mid,char* trg,char* var,char* min,char* max, char* crc250,int* irc, int len1,int len2,int len3,int len4,int len5);
void mod_adddefault_(int* mid,char* trg,char* val, char* crc250,int* irc, int len1,int len2,int len3);
void mod_pushdefault_(int* mid,char* crc250,int* irc, int len1);
void mod_addexpression_(int* mid, char* modName, char* obsExpr, char* smin, char* smax, char* crc250, int* irc, int len1, int len2, int len3, int len4, int len5);

void obs_opensession_(int* bid,char* crc250,int* irc, int len1);
void obs_clearfilestack_(int* bid,char* crc250,int* irc, int len1);
void obs_cleartargetstack_(int* bid,char* crc250,int* irc, int len1);
void obs_closesession_(int* bid,char* crc250,int* irc, int len1);
void obs_loadcache_(int* bid,char* path,char* crc250,int* irc, int len1,int len2);
void obs_makecache_(int* bid,char* path,char* crc250,int* irc, int len1,int len2);
void obs_peekfile_(int* bid,int* maxrep,int* nrep,char* rep250,char* crc250,int* irc,int len1,int len2);
void obs_peekfilelen_(int* bid,int* maxrep,char* crc250,int* irc, int len1);
void obs_popfile_(int* bid,char* path250,char* crc250,int* irc, int len1,int len2);
void obs_pushfile_(int* bid,char* path,char* crc250,int* irc, int len1,int len2);
void obs_pushtarget_(int* bid,char* trg,char* pos,char* descr,char* info,char* min,char* max, char* crc250,int* irc, int len1,int len2,int len3,int len4,int len5,int len6,int len7);
void obs_settablepath_(int* bid,char* path250,char* crc250,int* irc, int len1,int len2);
void obs_setindex_(int* bid,char* trg80,char* exp250, char* crc250,int* irc, int len1,int len2,int len3);
void obs_setindexlimits_(int* bid,char* s25,char* e25,char* crc250,int* irc, int len1,int len2,int len3);
void obs_setbufrtype_(int* bid,int* bufrType, int* subType, char* crc250,int* irc, int len1);

void col_opensession_(int* cid, char* crc250, int* irc, int len1);
void col_makexml_(int* cid, int* mid, int* bid, char* crc250, int* irc, int len1);
void col_closesession_(int* cid, char* crc250, int* irc, int len1);
void col_expression_(char* exp250, char* crc250, int* irc, int len1, int len2);
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
      PUSHs(sv_2mortal(newSViv(mid)));
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
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
      PUSHs(sv_2mortal(newSViv(sid)));
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
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
      PUSHs(sv_2mortal(newSViv(cid)));
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
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
#  "xs_makeModelCache" writes the model cache to a file.
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
#  "xs_makeObsCache" writes the observation cache to a file.
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
      obs_makecache_(&sid, path250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);

#
#  "xs_loadModelCache" loads the model cache from a file.
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
      mod_loadcache_(&sid, path250, crc250, &irc, 250, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(path250);
      free(crc250);

#
#  "xs_loadObsCache" loads the observation cache from a file.
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
      obs_loadcache_(&sid, path250, crc250, &irc, 250, 250);
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
#  "clearModelDefaultStack" clears the default stack
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_clearModelDefaultStack(int sid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      mod_cleardefaultstack_(&sid,crc250, &irc, 250);
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
#      (string) modelTargetName
#      (string) modelVariable
#      (string) min
#      (string) max
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_setModelIndex(int sid, char *variable);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      mod_setindex_(&sid,variable,crc250, &irc, 25, 25, 250);
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
#  "addModelDefault"  sets the model targets
#      (string) modelTargetName
#      (string) value
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_addModelDefault(int mid, char *modName, char *val);
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
      mod_adddefault_(&mid, m80, v80, crc250, &irc, 80,80,250);
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
#  "pushModelDefault"  pushes the added default values to the stack
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_pushModelDefault(int mid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      mod_pushdefault_(&mid, crc250, &irc, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);

#
#  "addExpression"  sets the match rules connecting model and observation targets
#      (string) modelTargetName
#      (string) obsTarget expression
#      (string) min
#      (string) max
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_addExpression(int mid, char *modName, char *obsExpr, char *vmin, char *vmax);
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
      mod_addexpression_(&mid, m80, o250, l80, u80,crc250, &irc, 80,250,80,80,250);
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


################################# OBSERVATIONS ##########################
#
#  "setTablePath" clears the file stack and sets the variable config file.
#      (string) path to BUFR tables
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message

void
xs_setObsTablePath(int bid, char *path);
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
      obs_settablepath_(&bid, path250, crc250, &irc, 250, 250);
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
xs_clearObsFileStack(int bid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      obs_clearfilestack_(&bid,crc250, &irc, 250);
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
xs_pushObsFile(int bid, ...);
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
         obs_pushfile_(&bid, path250, crc250, &irc2, 250, 250);
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
xs_peekObsFile(int bid);
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
	obs_peekfilelen_(&bid,&maxrep,crc250,&irc,250);
        nrep=0;
        if (irc == 0) {
	  rep250 = calloc(sizeof(char), lenr*maxrep+1);
	  obs_peekfile_(&bid,&maxrep,&nrep,rep250,crc250,&irc,250,250);
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
xs_popObsFile(int bid, ...);
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
         obs_popfile_(&bid, path250, crc250, &irc2, 250, 250);
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
xs_pushObsTarget(int cid, char *target, char *pos, char *descr, char *info, char *vmin, char *vmax);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      obs_pushtarget_(&cid,target,pos,descr,info,vmin,vmax,crc250,&irc,strlen(target),strlen(pos),strlen(descr),strlen(info),strlen(vmin),strlen(vmax),250);
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
xs_setObsIndex(int cid, char *target, char *expr);
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
      obs_setindex_(&cid, t80, e250,crc250, &irc, 80,250,250);
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
xs_setObsBufrType(int cid, int bufrType, int subType);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
      irc=0;
      crc250 = calloc(sizeof(char), 250);
      strcpy(crc250,"");
      obs_setbufrtype_(&cid, &bufrType, &subType, crc250, &irc, 250);
      if(irc == 0) {
         strcpy(crc250,"");
      }
      EXTEND(SP, 2);
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(crc250);


#
#  "xs_colocXML" gets the next observation array
#   Return array:
#      (integer) error return code (0=ok).
#      (string)  error return message
#

void
xs_colocXML(int cid, int mid, int bid);
    PREINIT:
      int  irc;
      char *crc250;
    PPCODE:
        irc=0;
        crc250 = calloc(sizeof(char), 250);
        strcpy(crc250,"");
        col_makexml_(&cid,&mid,&bid,crc250,&irc,250);
        EXTEND(SP, 2);
        PUSHs(sv_2mortal(newSViv(irc)));
        PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
        free(crc250);

#
#  "xs_expression" evaluates expression
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
      PUSHs(sv_2mortal(newSVpv(exp250,strlen(exp250))));
      PUSHs(sv_2mortal(newSViv(irc)));
      PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
      free(exp250);
      free(crc250);

