c = get_config()

c.TerminalInteractiveShell.confirm_exit = False
c.TerminalIPythonApp.display_banner = False
c.TerminalIPythonApp.quick = True
c.InteractiveShellApp.exec_lines = [
    'from collections import *',
    'from itertools import *',
    'from matplotlib import pyplot as plt'
]
