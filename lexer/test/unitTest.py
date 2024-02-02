#! /bin/python3
# This is a test skeleton, modify testcases.py to add more tests

import subprocess
import threading
import queue
import sys
import testcases

def read_output(out, queue):
    for line in iter(out.readline, ''):
        queue.put(line)
    out.close()

def sml_cmd(proc, cmd):
    proc.stdin.write(cmd)
    proc.stdin.flush()

def main():
    numErr = 0
    cmd = ["sml"]
    proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, bufsize=1)

    # Queue to hold output lines
    q = queue.Queue()

    # Start a thread to continuously read the process's stdout
    t = threading.Thread(target=read_output, args=(proc.stdout, q))
    t.daemon = True
    t.start()

    # Compile the lexer
    sml_cmd(proc, 'CM.make "lexer/src/resources.cm";\n')

    # Wait for the specific compile success message
    compile_success = False
    while True:
        try:
            line = q.get(timeout=2)  # Adjust timeout as needed
            # print(line, end='')  # Debugging output
            if 'val it = true' in line:
                compile_success = True
                break
        except queue.Empty:
            print("Compilation output timeout.")
            break

    if not compile_success:
        print("Compilation failed.")
        sys.exit(1)

    print ("Compilation finished, continue to test ...")

    # Run test cases
    for filename, expected_output in testcases.test_cases:
        sml_cmd(proc, f'Parse.parse "{filename}";\n')
    
        print (f"Testing {filename} ...") 
        # Capture and check the output for each test case
        linenum = 0
        test_output = []
        while True:
            try:
                line = q.get(timeout=2)  # Adjust timeout as needed
                # print(line, end='')  # Debugging output
                if expected_output[linenum] not in line:
                    print(f"Expected {expected_output[linenum]} in {line}")
                    numErr += 1
                if 'EOF' in line:  # Assuming test case ends with 'val it = ...'
                    break
                if "uncaught exception" in line:
                    numErr +=1
                    break
                linenum += 1
            except queue.Empty:
                print("Test case output timeout.")
                break
            except Error:
                print("Exception occurred, check if expected output correct")
                break
                

    print (f"{numErr} error(s) found in test")
    # Cleanup
    proc.stdin.close()
    proc.terminate()
    proc.wait()
    if numErr > 0:
        sys.exit(1)

if __name__ == "__main__":
    main()
