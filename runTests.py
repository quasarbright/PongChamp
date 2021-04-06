"""
Template for an integration test runner

In test_dir, there should be x.extension files and x.out and x.err files

The tests assert that executing the code files produces the desired stdin and stderr.

For example, if in test_dir, we have foo.c, foo.out, foo.err, this script will assert
that running foo.c will write the contents of foo.out to stdout exactly and the contents 
of foo.err are a substring of what is written to stderr
"""
# TODO add stdin piping

import unittest
import os
import subprocess

######################### customize these per language #########################

extension = ".js"

test_dir = os.path.join(".","examples")

build_cmd = ["stack", "build"]

def run_cmd(path):
	"""Generates the command to run a file at `path`
	"""
	return ["python", os.path.join(".", "bin", "PongChamp"), path]

######################### leave these be #########################

class IntegrationTests(unittest.TestCase):
    def run_test_file(self, filename):
        path = os.path.join(test_dir, filename)
        out_path = path+".out"
        err_path = path+".err"
        is_out = os.path.isfile(out_path)
        is_err = os.path.isfile(err_path)
        if is_out or is_err:
            result = subprocess.run(run_cmd(path), capture_output=True)
            if is_out:
                with open(out_path) as out:
                    expected = out.read()
                    actual = result.stdout.decode("utf-8")
                    err = result.stderr.decode("utf-8")
                    self.assertEqual(expected, actual, msg=path+extension+"\n"+err)
            if is_err:
                with open(out_path) as err:
                    expected = err.read()
                    actual = result.stderr.decode("utf-8")
                    self.assertIn(expected, actual, msg=path+extension)
        else:
            print("no output file for", path)

if __name__ == "__main__":
    files = os.listdir(test_dir)
    filenames = [file[:-2] for file in files if file[-2:] == extension]
    for filename in filenames:
        def make_test(filename):
            return lambda self: self.run_test_file(filename)
        setattr(IntegrationTests, "test_"+filename, make_test(filename))
    subprocess.run(build_cmd)
    unittest.main()
