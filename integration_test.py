from os import listdir, system
from os.path import exists, isdir
import sys
import subprocess

# constant definition
WACC_PATH: str = "target/debug/wacc_36"
TEST_PATH: str = "wacc-examples-36"
REF_PATH: str = "wacc-examples-36/refCompile"

# type definition
class incorrect_test():
  def __init__(self, path: str, our_message: str, ref_message: str) -> None:
    self.path: str = path
    self.our_message: str = self.translate_message(our_message)
    self.ref_message: str = self.translate_message(ref_message)

  def translate_message(self, message: int) -> str:
    if message == 0:
      return "PASS"
    elif message == 100:
      return "SYNTAX ERROR"
    elif message == 200:
      return "SEMANTIC ERROR"
    else:
      return "UNKNOWN ERROR"

# variable defined for result output
run_test_cases: int = 0
actual_syntax_error_test_cases: int = 0
actual_semantic_error_test_cases: int = 0
expected_syntax_error_test_cases: int = 0
expected_semantic_error_test_cases: int = 0
incorrect_list: list[incorrect_test] = []

def get_total_test_cases(path: str = TEST_PATH) -> int:
  total_test_cases: int = 0
  for test_case in listdir(path):
    new_path = f"{path}/{test_case}"
    if isdir(new_path):
      total_test_cases += get_total_test_cases(new_path)
    elif new_path.endswith(".wacc"):
      total_test_cases += 1
  return total_test_cases

def running_our_single_test_cases(path: str) -> int:
  global run_test_cases, actual_syntax_error_test_cases, actual_semantic_error_test_cases
  result: int = subprocess.run([WACC_PATH, path], stdout=subprocess.PIPE).returncode
  print(f"running {path} in our compiler: ", end="")
  run_test_cases += 1
  if result == 0:
    print("PASS")
  elif result == 100:
    print("SYNTAX ERROR")
    actual_syntax_error_test_cases += 1
  elif result == 200:
    print("SEMANTIC ERROR")
    actual_semantic_error_test_cases += 1
  else:
    print("UNKNOWN ERROR")
    exit(1)
  return result
  
def running_ref_single_test_cases(path: str) -> int:
  global expected_syntax_error_test_cases, expected_semantic_error_test_cases
  output: str = subprocess.run([REF_PATH, "-s", path], stdout=subprocess.PIPE).stdout.decode("utf-8")
  print(f"running {path} in reference compiler: ", end="")
  result: int = 0
  if output.find("Errors detected during compilation! Exit code 100 returned.") != -1:
    result = 100
    expected_syntax_error_test_cases += 1
    print("SYNTAX ERROR")
  elif output.find("Errors detected during compilation! Exit code 200 returned.") != -1:
    result = 200
    expected_semantic_error_test_cases += 1
    print("SEMANTIC ERROR")
  else:
    print("PASS")
  return result

def running_test_cases(path: str) -> None:
    if path.endswith("wacc"):
      running_our_single_test_cases(path)
      running_ref_single_test_cases(path)
      return
    for test_case in listdir(path):
      new_path = f"{path}/{test_case}"
      if isdir(new_path):
        running_test_cases(new_path)
      elif new_path.endswith(".wacc"):
        print("============================================================")
        our_result = running_our_single_test_cases(new_path)
        ref_result = running_ref_single_test_cases(new_path)
        if our_result != ref_result:
          incorrect_list.append(incorrect_test(new_path, our_result, ref_result))
          print("result not match")

# accepting one arguments as the path of the test directory (can be single file)
if __name__ == "__main__":
  if len(sys.argv) >= 2:
    path = f"{sys.argv[1]}"
  else:
    exit(0)
    
  # ensure that the wacc_36 executable exists
  if not exists(WACC_PATH):
    print("running cargo build")
    system("cargo build")

  # ensure that the test directory exists
  assert exists(TEST_PATH) == True, "test directory does not exist"

  # run the test cases
  running_test_cases(path)

  # print the error test cases
  print("======================== RESULTS ===========================")
  print("====================== error tests =========================")
  for test in incorrect_list:
    print(f"in {test.path}: out result: {test.our_message}, ref result: {test.ref_message}")

  # print the result
  print("============================================================")
  print(f"test cases have run: {run_test_cases}")
  print(f"actual passed test cases: {run_test_cases - actual_syntax_error_test_cases - actual_semantic_error_test_cases}")
  print(f"expected passed test cases: {run_test_cases - expected_syntax_error_test_cases - expected_semantic_error_test_cases}")
  print(f"actual syntax error test cases: {actual_syntax_error_test_cases}")
  print(f"expected syntax error test cases: {expected_syntax_error_test_cases}")
  print(f"actual semantic error test cases: {actual_semantic_error_test_cases}")
  print(f"expected semantic error test cases: {expected_semantic_error_test_cases}")
  print(f"remaining test cases: {get_total_test_cases() - run_test_cases}")
  print("============================================================")
  incorrect_number = len(incorrect_list)
  print(f"incorrect tests: {incorrect_number} OUT OF {run_test_cases}")
  print(f"local correctness percentage: {round((run_test_cases - incorrect_number) / run_test_cases * 100, 4)}%")

  if actual_syntax_error_test_cases != expected_syntax_error_test_cases:
    print("Syntax error test cases not match!")

  if actual_semantic_error_test_cases != expected_semantic_error_test_cases:
    print("Semantic error test cases not match!")
  
  assert actual_syntax_error_test_cases == expected_syntax_error_test_cases, "syntax error test cases not match"
  assert actual_semantic_error_test_cases == expected_semantic_error_test_cases, "semantic error test cases not match"