#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
import fnmatch
import commands
import tempfile
import subprocess
import shutil
import time
import signal
import platform
from optparse import OptionParser


class DirectoryJumper:
    def __init__(self,ppath):
        self._path=ppath
        self._base_path=os.getcwd()
        os.chdir(self._path)
        pass
    def __del__(self):
        os.chdir(self._base_path)
    pass

#-------------------------------------------------------------------------------

def options_script(argv):

    parser = OptionParser(usage="usage: %prog [options]")

    parser.add_option("-r", "--result", dest="result_dir", type="string",
                      metavar="<result_dir>",
                      help="choose results directory")

    parser.add_option("-j", "--jdd", dest="jdd", type="string",
                      metavar="<jdd>",
                      help="choose jdd file")

    parser.set_defaults(result_dir=os.getcwd())
    parser.set_defaults(src_dir=None)
    parser.set_defaults(jdd="TritonAMR.data")
   
    (options, args) = parser.parse_args(argv)

    if len(args) > 0:
        parser.print_help()
        sys.exit(1)
        pass

    if options.result_dir != os.getcwd():
        options.result_dir=os.path.join(os.getcwd(),options.result_dir)
        if not os.path.isdir(options.result_dir):
            os.mkdir(options.result_dir)
            pass
        pass

    result_dir = os.path.expanduser(options.result_dir)
    result_dir = os.path.expandvars(result_dir)
    result_dir = os.path.abspath(result_dir)
    if not os.path.isdir(result_dir):
        sys.stderr.write('Error: result dir \"' + result_dir + '\" is not a directory.\n')
        sys.exit(1)
        pass

    src_dir = options.src_dir
    if src_dir!=None:
        os.path.expanduser(options.src_dir)
        src_dir = os.path.expandvars(src_dir)
        src_dir = os.path.abspath(src_dir)
        if not os.path.isdir(src_dir):
            sys.stderr.write('Error: source dir \"' + src_dir + '\" is not a directory.\n')
            sys.exit(1)
            pass
        pass

    jdd = os.path.expanduser(options.jdd)
    jdd = os.path.expandvars(jdd)
    jdd = os.path.abspath(jdd)
    if not os.path.isfile(jdd):
        sys.stderr.write('Error: jdd \"' + jdd + '\" does not exist.\n')
        sys.exit(1)
        pass
 
    return jdd, result_dir, src_dir

#-------------------------------------------------------------------------------

def init_verify_env_tritonAMR():

    tritonAMR_home=os.getenv("TritonAMR_HOME")
    
    if not tritonAMR_home:
        sys.stderr.write('Error: TritonAMR_HOME is not defined.\n')
        sys.exit(1)
        pass

    lib_dir_tritonAMR=os.path.join(tritonAMR_home,"lib")
    if not os.path.isdir(lib_dir_tritonAMR):
        sys.stderr.write('Error: library directory \"' + lib_dir_tritonAMR + '\" is not a directory.\n')
        sys.exit(1)
        pass    

    exec_tritonAMR=os.path.join(tritonAMR_home,"bin","tritonAMR")
    if not os.path.isfile(exec_tritonAMR):
        sys.stderr.write('Error: binary \"' + exec_tritonAMR + '\" does not exist.\n')
        sys.exit(1)
        pass    

    return tritonAMR_home, lib_dir_tritonAMR, exec_tritonAMR

def run_tritonAMR(jdd, result_dir, src_dir, lib_dir_tritonAMR, exec_tritonAMR):

    jdd_cp="cp -f %s %s\n"%(jdd,result_dir)
    prepareFile="prepareDepFiles.tmp" ; prepareFileAbs=os.path.join(result_dir,prepareFile)
    f=file(prepareFileAbs,"w") ; f.write(jdd_cp) ; del f

    proc=subprocess.Popen(["sh",prepareFileAbs],stdout=subprocess.PIPE,stderr=subprocess.PIPE) ; so,se=proc.communicate()

    dj=DirectoryJumper(result_dir)
    st=" (time "+exec_tritonAMR+")"

    f=file(os.path.join(result_dir,"tritonAMR.log"),"w")
    f.close()
    st+=" 2>&1 | tee -a tritonAMR.log"
    proc=subprocess.Popen(st,shell=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE,
                          env={"LD_LIBRARY_PATH":lib_dir_tritonAMR}) 
    ptail=subprocess.Popen(["tail","-f","tritonAMR.log","--pid="+str(proc.pid)],stdout=sys.stdout,stderr=sys.stderr)
    so,se=proc.communicate()
    subprocess.call(["rm","-f",prepareFileAbs])    
    del dj
    return

#-------------------------------------------------------------------------------
# Main
#-------------------------------------------------------------------------------
def main(argv):
    """
    Main function.
    """

    jdd, result_dir, src_dir = options_script(argv)
    tritonAMR_home, lib_dir_tritonAMR, exec_tritonAMR=init_verify_env_tritonAMR()
    run_tritonAMR(jdd, result_dir, src_dir, lib_dir_tritonAMR, exec_tritonAMR)

    sys.exit(0)
    
    pass

if __name__ == "__main__":
    main(sys.argv[1:])

#-------------------------------------------------------------------------------
# End
#-------------------------------------------------------------------------------
