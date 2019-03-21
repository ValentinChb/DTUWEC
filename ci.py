import pytest
import sys
import os
import subprocess
"""
python ci.py <platform> [<configuration>] [push]
"""

if __name__ == '__main__':
    module = 'DTU_Controller'
    sln = 'src/dtu_controller.sln'
    dll_lst = ['src/dtu_we_controller/%s/%s/dtu_we_controller%s.dll','src/dtu_we_controller_bladed/%s/%s/dtu_we_controller_bladed%s.dll']
    binary_folder = 'control-%s'
    
    def run(cmd):
        print(cmd)
        if os.system(cmd):
            raise Exception("'%s' failed" % cmd)

    def run_git_cmd(cmd):
        git_repo_path = os.getcwd()
        if not os.path.isdir(os.path.join(git_repo_path, ".git")):
            raise Warning("'%s' does not appear to be a Git repository." % git_repo_path)
        try:
            process = subprocess.Popen(cmd,
                                       stdout=subprocess.PIPE,
                                       stderr=subprocess.PIPE,
                                       universal_newlines=True,
                                       cwd=os.path.abspath(git_repo_path))
            stdout, stderr = process.communicate()
            if process.returncode != 0:
                raise EnvironmentError("%s\n%s" % (stdout, stderr))
            return stdout.strip()

        except EnvironmentError as e:
            raise e
            raise Warning("unable to run git")


    platform = sys.argv[1]
    if len(sys.argv) == 3 and sys.argv[2].lower() != 'push':
        conf = sys.argv[2]
    else:
        conf = 'Release'
    push = sys.argv[-1] == 'push'
    ext = ['', '_64'][platform.lower() == 'x64']
    
    binary_folder = binary_folder%platform
    print("Run ci")
    print("- Platform: %s" % platform)
    print("- Solution file: %s" % (sln))
    for dll in dll_lst:
        print("- dll: " + dll % (platform, conf, ext))
    print("-" * 20)

    run('devenv %s /rebuild "%s|%s"' % (sln, conf, platform))
    #res = pytest.main(['.'])
    #if res:
    #    sys.exit(res)

    run("git clone -b master --depth 1 git@gitlab.windenergy.dtu.dk:OpenLAC/control-binary/%s.git" % binary_folder)
    
    tag = "%-12s"%run_git_cmd("git describe --tags")
    message = run_git_cmd("git tag -n99 %s"%tag).replace(tag, "").strip()
    date = run_git_cmd('git log -1 --format=%%ai --tags %s'%tag)[:10].strip()
    name = run_git_cmd('git log -1 --format="%%an" --tags %s'%tag).strip()
    with open('%s/changelog.txt'%binary_folder,'a') as fid:
        info = "  ".join([tag, date, name, module])
        message = ("\n"+" "*60).join([l.strip() for l in message.split("\n")])
        fid.write("%-60s%s\n"%(info[:58], message))
    
    for dll in dll_lst:
        run("%s %s/update_module.py %s" % (sys.executable, binary_folder, dll%(platform, conf, ext)))
    
    if push:
        run("%s %s/update_module.py push" % (sys.executable, binary_folder))
