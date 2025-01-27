from probability.warhammer.test_attack import TestRunner
import subprocess
import sys
import json
from pathlib import Path
from typing import IO

def run_tests(calculator_cmd: str) -> None:
    """
    Run tests using specified probability calculator command
    
    calculator_cmd: Command to run the probability calculator
        e.g. "python calc.py" or "./calculator" or "java Calculator"
    """

    print(f"Running tests with calculator: {calculator_cmd}")
    
    # Start the probability calculator process
    calc_process = subprocess.Popen(
        calculator_cmd.split(),
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    
    print(f"Calculator process started with PID: {calc_process.pid}")
    
    print(f"Running test process")
    # Run the test process
    test_process = subprocess.Popen(
        [sys.executable, str(Path(__file__).parent / "test_attack.py")],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=None,
        text=True
    )

    print(f"Test process started with PID: {test_process.pid}")
    calc_stdin : IO[str] = calc_process.stdin #type: ignore
    calc_stdout : IO[str] = calc_process.stdout #type: ignore
    test_stdin : IO[str] = test_process.stdin #type: ignore
    test_stdout : IO[str] = test_process.stdout #type: ignore
    
    # Pipe test cases through the calculator
    index =0 
    while True:
        index += 1
        test_case = test_stdout.readline() 
        if not test_case:
            print(f"Test process finished")
            break
        print(f"Running test case {index}")
        # Send to calculator
        calc_stdin.write(test_case) 
        calc_stdin.flush() 
        
        # Get result from calculator
        result: str = calc_stdout.readline() 
        
        # Send back to test process
        test_stdin.write(result) 
        test_stdin.flush() 
    
    # Clean up
    calc_process.terminate()
    test_process.terminate()

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python run_tests.py <calculator_command>")
        sys.exit(1)
    
    run_tests(" ".join(sys.argv[1:])) 