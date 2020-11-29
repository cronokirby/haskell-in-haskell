'''
This file runs all of the integration tests in the directory `integration_tests`
'''
import os
import re
import subprocess

ROOT = 'integration_tests'
OUTPUT = '.output.c'


def get_expected(file_name):
    '''
    Return the expected output for a given file.
    '''
    expected = []
    with open(file_name) as fp:
        for line in fp:
            match = re.search(r'--.*OUT\((.*)\)', line)
            if match:
                expected.append(match.group(1))
    return '\n'.join(expected)


def gen_executable(file_name):
    '''
    Generate an executable for a given file
    '''
    out = subprocess.check_output(f"cabal run haskell-in-haskell -- compile {file_name} .output.c", shell=True).decode('utf-8')
    if 'Error' in out:
        raise Exception('\n'.join(out.split('\n')[1:]))
    subprocess.run("gcc -std=c99 .output.c", shell=True, check=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)



def get_output(file_name):
    '''
    Get the output for a given file
    '''
    gen_executable(file_name)
    return subprocess.check_output("./a.out", shell=True).decode('utf-8')


def file_names():
    '''
    Yield all of the file names we need to run an integration test on
    '''
    for file_name in os.listdir(ROOT):
        if file_name.endswith('.hs'):
            yield os.path.join(ROOT, file_name)


def main():
    '''
    The main function for our script.

    This will find all of the integration tests to run, and then
    run them in order, checking that their expected output matches
    what gets printed.
    '''
    for name in file_names():
        expected = get_expected(name).strip()
        try:
            actual = get_output(name).strip()
        except BaseException as err:
            print(f'{name}: FAIL')
            print('Exception:')
            print(err)
            continue
        if expected == actual:
            print(f'\033[1m{name}\033[0m:\t\033[1m\033[32mPASS\033[0m')
        else:
            print(f'\033[1m{name}\033[0m:\t\033[1m\033[31mFAIL\033[0m')
            print('Expected:\033[1m')
            print(expected)
            print('\033[0m\nBut Found:\033[1m')
            print(actual)
            print('\033[0m')


if __name__ == '__main__':
    main()
