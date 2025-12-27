#!/usr/bin/env python3
import os
import re

def parse_latency_to_microseconds(value_str):
    """
    Parses a latency string (e.g., '1.23ms', '2.5s', '500us') and converts it to microseconds.
    Returns the value as a float or the original string if parsing fails.
    """
    value_str = value_str.strip().lower()
    try:
        if 'ms' in value_str:
            return float(value_str.split('ms')[0]) * 1000
        elif 'us' in value_str:
            return float(value_str.split('us')[0])
        elif 's' in value_str:
            return float(value_str.split('s')[0]) * 1000 * 1000
        else:
            # Assumes seconds if no unit is present, which might happen for values like '0'
            return float(value_str) * 1000 * 1000
    except (ValueError, TypeError):
        return value_str # Return as is if it's not a parsable number, e.g., "N/A"

def analyze_log_file(log_path):
    """
    Analyzes a single log file to extract latency and requests/sec for different connection counts.
    """
    system_name = os.path.basename(log_path).replace('.log', '')
    results = {
        'latency': {'10': 'N/A', '100': 'N/A', '400': 'N/A'},
        'requests': {'10': 'N/A', '100': 'N/A', '400': 'N/A'}
    }
    current_connection = None

    with open(log_path, 'r', encoding='utf-8') as f:
        for line in f:
            # Check for section headers that define the connection count
            if line.startswith('#') and '-c' in line:
                match = re.search(r'-c\s+(\d+)', line)
                if match:
                    connection_count = match.group(1)
                    # Check if the extracted count is one of the keys we are tracking
                    if connection_count in results['latency']:
                        current_connection = connection_count
                continue

            if not current_connection:
                continue

            # Extract Latency from lines like:
            # "  Latency     2.41ms    1.17ms   6.55ms    83.17%"
            # "  Latency    32.55ms,  4.19ms,  49.03ms,    8.69ms"
            if "Latency" in line:
                parts = line.split()
                try:
                    # Find the 'Latency' token and get the next value (Avg)
                    latency_index = parts.index("Latency")
                    avg_latency_str = parts[latency_index + 1].replace(',', '')
                    results['latency'][current_connection] = parse_latency_to_microseconds(avg_latency_str)
                except (ValueError, IndexError):
                    continue

            # Extract Requests/sec from lines like:
            # "  Requests/sec:  41433.15"
            req_match = re.search(r'Requests/sec:\s*([\d\.]+)', line)
            if req_match:
                # Store as float for consistent formatting later if needed
                results['requests'][current_connection] = float(req_match.group(1))
    
    return system_name, results

def main():
    """
    Main function to process all log files and generate TSV reports.
    """
    # Find all files ending with .log in the current directory
    try:
        log_files = sorted([f for f in os.listdir('.') if f.endswith('.log')])
    except OSError as e:
        print(f"Error reading directory: {e}")
        return
        
    if not log_files:
        print("No .log files found in the current directory.")
        return

    all_latency_data = []
    all_requests_data = []

    # Process each log file
    for log_file in log_files:
        try:
            system_name, results = analyze_log_file(log_file)
            all_latency_data.append((system_name, results['latency']))
            all_requests_data.append((system_name, results['requests']))
        except Exception as e:
            print(f"Could not process file {log_file}: {e}")

    # Define TSV header, matching the requested order
    header = '"system name"\t"100 connections"\t"10 connections"\t"400 connections"\n'

    # Write Latency TSV file
    try:
        with open('latency.tsv', 'w', encoding='utf-8') as f:
            f.write(header)
            for name, data in all_latency_data:
                # Ensure data is written in the correct column order
                f.write(f'"{name}"\t{data["100"]}\t{data["10"]}\t{data["400"]}\n')
    except IOError as e:
        print(f"Error writing to latency.tsv: {e}")


    # Write Requests/sec TSV file
    try:
        with open('requests-per-sec.tsv', 'w', encoding='utf-8') as f:
            f.write(header)
            for name, data in all_requests_data:
                # Ensure data is written in the correct column order
                f.write(f'"{name}"\t{data["100"]}\t{data["10"]}\t{data["400"]}\n')
    except IOError as e:
        print(f"Error writing to requests-per-sec.tsv: {e}")

    print("Successfully generated 'latency.tsv' and 'requests-per-sec.tsv'.")

if __name__ == '__main__':
    main()
