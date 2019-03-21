import pytest
import sys
import os
"""
python ci.py <platform> [<configuration>] [push]
"""

if __name__ == '__main__':
    
    sln = 'src/dtu_controller.sln'
    dll_lst = ['src/dtu_we_controller/%s/%s/dtu_we_controller%s.dll','src/dtu_we_controller_bladed/%s/%s/dtu_we_controller_bladed%s.dll']
    
    def run(cmd):
        print(cmd)
        if os.system(cmd):
            raise Exception("'%s' failed" % cmd)

    platform = sys.argv[1]
    if len(sys.argv) == 3 and sys.argv[2].lower() != 'push':
        conf = sys.argv[2]
    else:
        conf = 'Release'
    push = sys.argv[-1] == 'push'
    ext = ['', '_64'][platform.lower() == 'x64']
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

    run("git clone -b master --depth 1 git@gitlab.windenergy.dtu.dk:OpenLAC/control-binary/control-%s.git" % platform)
    for dll in dll_lst:
        run("%s control-%s/update_module.py %s" % (sys.executable, platform, dll%(platform, conf, ext)))
    if push:
        run("%s control-%s/update_module.py push" % (sys.executable, platform))
