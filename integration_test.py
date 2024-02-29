from os import listdir, system
from os.path import exists, isdir, basename  
import sys
import subprocess
import platform

# constant definition
WACC_PATH: str = "target/debug/wacc_36"
TEST_PATH: str = "wacc-examples-36"
REF_PATH: str = "wacc-examples-36/refCompile"
OUT_PATH: str = "out"


# type definition
class Result:
    def __init__(self) -> None:
        self.exit_code: int = 0
        self.output: str = ""

    def __str__(self) -> str:
        return f"exit code: \"{self.exit_code}\", output: \"{self.output}\""

    def output_cmp(self, this: str, other: str) -> bool:
        if other.find("runtime_error") != -1:
            return True
        countself: int = 0
        countother: int = 0
        while len(this) != countself and len(other) != countother:
            if other[countother:countother+7] == '#addrs#':
                countother += 7
                countself += 14
                continue
            if other[countother:countother+6] == '#addrs':
                countother += 6
                countself += 13
                continue
            if other[countother:countother+6] == 'addrs#':
                countother += 6
                countself += 13
                continue
            if this[countself] != other[countother]:
                return False
            countself += 1
            countother += 1
        return len(this) == countself and len(other) == countother

    def __eq__(self, other) -> bool:
        return self.exit_code == other.exit_code and self.output_cmp(self.output, other.output)
        

class incorrect_test():
    def __init__(self, path: str, our_message: Result, ref_message: Result) -> None:
        self.path: str = path
        self.our_message: Result = our_message
        self.ref_message: Result = ref_message

# variable defined for result output
run_test_cases: int = 0
actual_syntax_error_test_cases: int = 0
actual_semantic_error_test_cases: int = 0
expected_syntax_error_test_cases: int = 0
expected_semantic_error_test_cases: int = 0
incorrect_list: list[incorrect_test] = []


# only return bool if it is x86-64 linux
def check_systyem() -> bool:
    return platform.system() == "Linux" and platform.machine() != "arm"

def get_file_content(path: str) -> str:
    with open(path, "r") as file:
        return file.read()

def get_input_content(path: str) -> str:
    content = get_file_content(path)
    index = content.find("Input:")
    if index == -1:
        return ""
    ans = content[index + 7:].split("\n")[0]
    return ans if ans != [] else ""

def get_output_content(path: str) -> str:
    content = get_file_content(path)
    index = content.find("Output:")
    index_end = content.find("Program:")
    index_end_if_exit = content.find("Exit:")
    if index == -1:
        return ""
    # if there is an exit statement, we only want to get the output before the exit statement
    if index_end_if_exit != -1 and (index_end_if_exit < index_end or index_end == -1):
        index_end = index_end_if_exit
    if index_end == -1:
        index_end = content.find("begin") # as some program does not contain program: at the start
    output = content[index + 7:index_end].replace("\n# ", "\n").replace("\n#", "\n").strip()
    return output

def get_exit_code(path: str) -> int:
    content = get_file_content(path)
    index = content.find("Exit:")
    if index == -1:
        return 0
    index_end = content.find("begin") # as some program does not contain program: at the start
    index_end_if_program = content.find("Program:")
    if index_end_if_program != -1 and index_end_if_program < index_end:
        index_end = index_end_if_program
    return int(content[index + 5:index_end].replace("\n# ", "").strip())

def get_total_test_cases(path: str = TEST_PATH) -> int:
    total_test_cases: int = 0
    for test_case in listdir(path):
        new_path = f"{path}/{test_case}"
        if isdir(new_path):
            total_test_cases += get_total_test_cases(new_path)
        elif new_path.endswith(".wacc"):
            total_test_cases += 1
    return total_test_cases


def running_our_single_test_cases(path: str) -> Result:
    global run_test_cases, actual_syntax_error_test_cases, actual_semantic_error_test_cases
    result: Result = Result()
    outputwacc: str = f"{OUT_PATH}/{basename(path)}"
    output: str = outputwacc[:outputwacc.find(".wacc")]

    compiler_res: int = subprocess.run([WACC_PATH, path, f"{output}.s"], stdout=subprocess.PIPE).returncode

    print(f"running {path} in our compiler: ", end="")
    run_test_cases += 1
    if compiler_res == 100:
        print("SYNTAX ERROR")
        result.exit_code = compiler_res
        actual_syntax_error_test_cases += 1
    elif compiler_res == 200:
        print("SEMANTIC ERROR")
        result.exit_code = compiler_res
        actual_semantic_error_test_cases += 1
    else:
        print("PASS")
        if exists(f"{output}.s"):
            subprocess.run(["gcc", f"{output}.s", "-o", output, "-z", "noexecstack"], stdout=subprocess.PIPE)
            res = subprocess.run([f"./{output}"], input=(get_input_content(path) + "\n").encode("utf-8"), stdout=subprocess.PIPE)
            result.exit_code = res.returncode
            result.output = res.stdout.decode("utf-8").strip()
    return result

def reduce_line_breaks(content: str) -> str:
    separator = "==========================================================="
    content = content[content.find(separator) + len(separator):]
    content = content[:content.find(separator)]
    # replace "num content" to "content" where num is integer
    count: int = 0
    while content.find(f"\n{count}\t") != -1:
        content = content.replace(f"\n{count}\t", "\n")
        count += 1
    return content[1:]

def get_all_assembly_code(path: str) -> int:
    output: str = subprocess.run([REF_PATH, "-t", "x86-64", "-a", path], stdout=subprocess.PIPE).stdout.decode("utf-8")
    print(f"running {path} in reference compiler: ", end="")
    with open(f"{OUT_PATH}/{basename(path)}.s", "w") as file:
        file.write(reduce_line_breaks(output))


def running_ref_single_test_cases(path: str) -> Result:
    global expected_syntax_error_test_cases, expected_semantic_error_test_cases
    # output: str = subprocess.run([REF_PATH, "-s", path], stdout=subprocess.PIPE).stdout.decode("utf-8")
    print(f"running {path} in reference compiler: ", end="")
    result: Result = Result()
    if path.find("syntaxErr") != -1:
        result.exit_code = 100
        expected_syntax_error_test_cases += 1
        print("SYNTAX ERROR")
    elif path.find("semanticErr") != -1:
        result.exit_code = 200
        expected_semantic_error_test_cases += 1
        print("SEMANTIC ERROR")
    else:
        result.exit_code = get_exit_code(path)
        result.output = get_output_content(path)
        print("PASS")
    return result

def run_each_test_case(path: str) -> None:
    print("============================================================")
    # get_all_assembly_code(path)
    our_result: Result = running_our_single_test_cases(path)
    ref_result: Result = running_ref_single_test_cases(path)
    # print(our_result, ref_result)
    if our_result != ref_result:
        incorrect_list.append(incorrect_test(path, our_result, ref_result))
        print("result not match")

def running_test_cases(path: str) -> None:
    if path.endswith("wacc"):
        run_each_test_case(path)
        return
    for test_case in listdir(path):
        new_path = f"{path}/{test_case}"
        if isdir(new_path):
            running_test_cases(new_path)
        elif new_path.endswith(".wacc"):
            run_each_test_case(new_path)


# accepting arguments as the path of the test directory (can be single file)
if __name__ == "__main__":
    arg_items = len(sys.argv)
    if arg_items < 2:
        exit(0)
    # ensure that the wacc_36 executable exists
    if not exists(WACC_PATH):
        print("running cargo build")
        system("cargo build")

    if not exists(OUT_PATH):
        system(f"mkdir {OUT_PATH}")

    # ensure that the test directory exists
    assert exists(TEST_PATH), "test directory does not exist"

    # run the test cases
    for i in range(1, arg_items):
        path = f"{sys.argv[i]}"
        running_test_cases(path)

    total_test = get_total_test_cases()

    # print the error test cases
    print("======================== RESULTS ===========================")
    print("====================== error tests =========================")
    for test in incorrect_list:
        print(f"in {test.path}: out result: {test.our_message}, ref result: {test.ref_message}")

    # print the result
    print("============================================================")
    print(f"test cases have run: {run_test_cases}")
    print(
        f"actual passed test cases: {run_test_cases - actual_syntax_error_test_cases - actual_semantic_error_test_cases}")
    print(
        f"expected passed test cases: {run_test_cases - expected_syntax_error_test_cases - expected_semantic_error_test_cases}")
    print(f"actual syntax error test cases: {actual_syntax_error_test_cases}")
    print(f"expected syntax error test cases: {expected_syntax_error_test_cases}")
    print(f"actual semantic error test cases: {actual_semantic_error_test_cases}")
    print(f"expected semantic error test cases: {expected_semantic_error_test_cases}")
    print(f"remaining test cases: {total_test - run_test_cases}")
    print("============================================================")
    incorrect_number = len(incorrect_list)
    print(f"incorrect tests: {incorrect_number} OUT OF {run_test_cases}")
    print(f"local correctness percentage: {round((run_test_cases - incorrect_number) / run_test_cases * 100, 4)}%")
    print(f"test coverage: {run_test_cases}/{total_test}, which is {round((run_test_cases / total_test) * 100, 4)}%")

    if actual_syntax_error_test_cases != expected_syntax_error_test_cases:
        print("Syntax error test cases not match!")

    if actual_semantic_error_test_cases != expected_semantic_error_test_cases:
        print("Semantic error test cases not match!")

    assert actual_syntax_error_test_cases == expected_syntax_error_test_cases, "syntax error test cases not match"
    assert actual_semantic_error_test_cases == expected_semantic_error_test_cases, "semantic error test cases not match"
