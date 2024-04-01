#! /bin/python3

import os
import sys
import subprocess

class TestPipeline:
    def __init__(self):
        self.numErr = 0
        self.numTests = 0

    def test_file(self, filedir):
        self.numTests += 1
        command = f"echo -n {filedir} | sml test.sml"
        ps = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        return ps

    def test_positive(self, filedir):
        ps = self.test_file(filedir)
        stdout, stderr = ps.communicate()
        if ps.returncode != 0:
            print(f"Error occurred in checking postive case: {filedir}")
            self.numErr += 1


    def test_negative(self, filedir):
        # print stdoud and stderr of ps
        ps = self.test_file(filedir)
        stdout, stderr = ps.communicate()
        if ps.returncode == 0:
            print(f"Error not detected in checking negative case: {filedir}")
            self.numErr += 1

    def test_negative_folder(self, folder):
        for subdir, _, files in os.walk(folder):
            for file in files:
                filedir = os.path.join(subdir, file)
                if(filedir.endswith(".tig")):
                    self.test_negative(filedir)

    def test_positive_folder(self, folder):
        for subdir, _, files in os.walk(folder):
            for file in files:
                filedir = os.path.join(subdir, file)
                if(filedir.endswith(".tig")):
                    self.test_positive(filedir)
    
    def run_tests(self, folder):
        self.test_negative_folder(os.path.join(folder, "negative"))
        self.test_positive_folder(os.path.join(folder, "positive"))
        if self.numErr == 0:
            print("All tests passed")
        else:
            print(f"{self.numErr}/{self.numTests} tests failed")
            exit(1)


if __name__ == "__main__":
    TestPipeline().run_tests(sys.argv[1])
