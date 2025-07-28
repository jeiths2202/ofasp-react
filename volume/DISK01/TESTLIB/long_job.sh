#!/bin/bash
# Long running job for SMBJOB/REFJOB demonstration

echo "Long job started at $(date)"
echo "Job parameters: $@"

# Simulate long running work
for i in {1..30}; do
    echo "Processing step $i of 30..."
    sleep 1
done

echo "Long job completed at $(date)"
exit 0