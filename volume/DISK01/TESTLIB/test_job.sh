#!/bin/bash
# Test job for SMBJOB/REFJOB demonstration

echo "Test job started at $(date)"
echo "Job parameters: $@"

# Simulate some work
for i in {1..5}; do
    echo "Processing step $i..."
    sleep 2
done

echo "Test job completed at $(date)"
exit 0