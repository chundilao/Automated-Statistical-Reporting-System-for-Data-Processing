import subprocess
import os
import sys

def main():
    # Get the directory where the .exe is located
    if getattr(sys, 'frozen', False):
        # Running as compiled .exe
        script_dir = os.path.dirname(sys.executable)
    else:
        # Running as .py script
        script_dir = os.path.dirname(os.path.abspath(__file__))
    
    # Change to the script directory
    os.chdir(script_dir)
    
    # Path to R.exe
    r_exe = os.path.join(script_dir, "R-Portable", "bin", "R.exe")
    
    # Build the R command
    r_command = '.libPaths(c("R-Portable/library", .libPaths())); shiny::runApp(".", launch.browser=TRUE)'
    
    # Run R
    subprocess.run([r_exe, "--no-save", "--slave", "-e", r_command])

if __name__ == "__main__":
    main()