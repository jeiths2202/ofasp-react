# MSGSample Troubleshooting Guide

## Quick Reference
This guide provides solutions for common issues encountered when executing the MSGSample program through the OpenASP AX CALL command.

## Common Issues and Solutions

### 1. Program Not Found Errors

#### Issue: "Program 'MSGSAMPLE' not registered in catalog.json"
**Symptoms**:
```
[ERROR] Program 'MSGSAMPLE' not registered in catalog.json
```

**Root Causes**:
- Program not properly registered in catalog.json
- Case sensitivity issues in catalog entry
- Volume or library path incorrect

**Solutions**:
1. **Verify Catalog Entry**:
   ```bash
   cd /home/aspuser/app/asp-manager/public/config
   grep -i "msgsample" catalog.json
   ```

2. **Check Required Fields**:
   ```json
   "MSGSample": {
     "TYPE": "PGM",
     "PGMTYPE": "JAVA",
     "PGMNAME": "MSGSample",
     "CLASSFILE": "MSGSample.class",
     "JARFILE": "MSGSample.jar",
     "DESCRIPTION": "COBOL to Java converted program",
     "VERSION": "1.0"
   }
   ```

3. **Restart ASP Manager** (if catalog was modified):
   ```bash
   cd /home/aspuser/app/asp-manager
   npm restart
   ```

#### Issue: "Program 'MSGSAMPLE' not found in any library"
**Symptoms**:
```
[ERROR] Program 'MSGSAMPLE' not found in any library on volume 'DISK01'
```

**Solutions**:
1. **Verify File Existence**:
   ```bash
   ls -la /home/aspuser/app/volume/DISK01/TESTLIB/MSGSample.*
   ```

2. **Check Library Structure**:
   ```bash
   find /home/aspuser/app/volume/DISK01 -name "*MSGSample*" -type f
   ```

3. **Verify Permissions**:
   ```bash
   chmod 644 /home/aspuser/app/volume/DISK01/TESTLIB/MSGSample.jar
   chmod 644 /home/aspuser/app/volume/DISK01/TESTLIB/MSGSample.class
   ```

### 2. File Access Issues

#### Issue: "Error: MSGSample.jar not found in current directory"
**Symptoms**:
```
Error: MSGSample.jar not found in current directory
```

**Root Causes**:
- JAR file missing from expected location
- File permissions prevent access
- Incorrect working directory

**Solutions**:
1. **Verify JAR File Location**:
   ```bash
   cd /home/aspuser/app/volume/DISK01/TESTLIB
   ls -la MSGSample.jar
   ```

2. **Check File Integrity**:
   ```bash
   jar tf MSGSample.jar | head -10
   ```

3. **Rebuild JAR if Corrupted**:
   ```bash
   # If you have source files
   jar cf MSGSample.jar com/cobol2java/
   ```

#### Issue: "Unable to access SAMDATA file"
**Symptoms**:
```
Exception in thread "main" java.io.FileNotFoundException: SAMDATA
```

**Solutions**:
1. **Verify Data File**:
   ```bash
   ls -la /home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA
   ```

2. **Check File Content**:
   ```bash
   head -5 /home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA
   ```

3. **Verify File Format**:
   - Should contain exactly 10 records
   - Each record should be 80 characters
   - Fixed-block format (FB)

4. **Restore from Backup** (if corrupted):
   ```bash
   # Sample data restoration
   cat > /home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA << 'EOF'
   100001JOHN SMITH           SALES     00035000        20220115                            
   100002MARY JOHNSON         MARKETING 00042000        20210308                            
   100003ROBERT BROWN         IT        00055000        20200612                            
   100004SUSAN DAVIS          HR        00038000        20220820                            
   100005MICHAEL WILSON       FINANCE   00048000        20190423                            
   100006JENNIFER GARCIA      SALES     00033000        20231201                            
   100007DAVID MARTINEZ       IT        00062000        20180905                            
   100008LISA ANDERSON        MARKETING 00045000        20210710                            
   100009JAMES TAYLOR         FINANCE   00051000        20200228                            
   100010PATRICIA THOMAS      HR        00040000        20220415                            
   EOF
   ```

### 3. Java Runtime Issues

#### Issue: "java: command not found"
**Symptoms**:
```
[ERROR] Java execution failed: java: command not found
```

**Solutions**:
1. **Install Java Runtime**:
   ```bash
   sudo apt update
   sudo apt install openjdk-11-jre
   ```

2. **Verify Java Installation**:
   ```bash
   java -version
   which java
   ```

3. **Set JAVA_HOME** (if needed):
   ```bash
   export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
   export PATH=$JAVA_HOME/bin:$PATH
   ```

#### Issue: "Java heap space errors"
**Symptoms**:
```
Exception in thread "main" java.lang.OutOfMemoryError: Java heap space
```

**Solutions**:
1. **Increase Heap Size**:
   ```bash
   # Modify CALL function to use more memory
   java -Xmx512m -jar MSGSample.jar
   ```

2. **Check Available Memory**:
   ```bash
   free -h
   ```

3. **Optimize JVM Settings**:
   ```bash
   java -XX:+UseG1GC -Xmx256m -jar MSGSample.jar
   ```

### 4. SMED Map Display Issues

#### Issue: "Incorrect display formatting"
**Symptoms**:
- Misaligned columns
- Missing field data
- Truncated output

**Solutions**:
1. **Verify SMED Map Definition**:
   ```bash
   cat /home/aspuser/app/volume/DISK01/TESTLIB/MSGSAMP1
   ```

2. **Check Terminal Settings**:
   - Ensure terminal supports 80-column display
   - Verify line wrapping settings
   - Check character encoding (UTF-8)

3. **Validate Field Positions**:
   ```bash
   # Check field alignment manually
   java -jar MSGSample.jar | cut -c1-20  # First 20 characters
   ```

#### Issue: "SMED map not found"
**Symptoms**:
```
[ERROR] SMED map 'MSGSAMP1' not found
```

**Solutions**:
1. **Verify Map File**:
   ```bash
   ls -la /home/aspuser/app/volume/DISK01/TESTLIB/MSGSAMP1
   ```

2. **Check Catalog Registration**:
   ```bash
   grep -A 10 "MSGSAMP1" /home/aspuser/app/asp-manager/public/config/catalog.json
   ```

3. **Restore Map Definition**:
   ```bash
   cat > /home/aspuser/app/volume/DISK01/TESTLIB/MSGSAMP1 << 'EOF'
   *SMED MAP: MSGSAMP1
   *DESCRIPTION: Display file map for MSGSample program
   *ROWS: 24
   *COLS: 80
   
   *FIELD DEFINITIONS
   FIELD NAME=HEADER POS=(1,1) LEN=80 ATTR=PROT COLOR=BLUE
   FIELD NAME=EMPDATA POS=(5,1) LEN=80 ATTR=UNPROT COLOR=GREEN
   FIELD NAME=STATUS POS=(23,1) LEN=80 ATTR=PROT COLOR=WHITE
   EOF
   ```

### 5. Performance Issues

#### Issue: "Program execution too slow"
**Symptoms**:
- Execution takes > 10 seconds
- System becomes unresponsive
- Timeout errors

**Solutions**:
1. **Check System Resources**:
   ```bash
   top -p `pgrep java`
   iostat -x 1 5
   ```

2. **Optimize Java Performance**:
   ```bash
   # Pre-compile if possible
   java -XX:+TieredCompilation -jar MSGSample.jar
   ```

3. **Reduce Concurrent Load**:
   - Limit simultaneous executions
   - Schedule heavy operations during off-peak hours

#### Issue: "Memory leaks detected"
**Symptoms**:
- Memory usage continuously increases
- System slows down over time
- Out of memory errors

**Solutions**:
1. **Monitor Memory Usage**:
   ```bash
   # Monitor during execution
   watch -n 1 'ps aux | grep java | grep MSGSample'
   ```

2. **Profile Memory**:
   ```bash
   java -XX:+PrintGCDetails -jar MSGSample.jar
   ```

3. **Restart Services Periodically**:
   ```bash
   # If persistent memory issues
   systemctl restart asp-manager
   ```

### 6. Network and Server Issues

#### Issue: "OpenASP AX server not responding"
**Symptoms**:
```
curl: (7) Failed to connect to localhost port 3005: Connection refused
```

**Solutions**:
1. **Check Server Status**:
   ```bash
   netstat -tlnp | grep :3005
   ps aux | grep node
   ```

2. **Restart ASP Manager**:
   ```bash
   cd /home/aspuser/app/asp-manager
   npm run build
   npm start
   ```

3. **Check Server Logs**:
   ```bash
   tail -f /home/aspuser/app/asp-manager/server/server.log
   ```

#### Issue: "Proxy errors in terminal"
**Symptoms**:
```
Proxy error: Could not proxy request /api/call
```

**Solutions**:
1. **Verify Backend Services**:
   ```bash
   curl http://localhost:8000/health
   ```

2. **Check Configuration**:
   ```bash
   cat /home/aspuser/app/asp-manager/package.json | grep proxy
   ```

3. **Restart Backend Services**:
   ```bash
   cd /home/aspuser/app/server
   python3 api_server.py &
   ```

## Diagnostic Commands

### System Health Check
```bash
#!/bin/bash
# system-health-check.sh

echo "=== OpenASP AX System Health Check ==="
echo "Date: $(date)"
echo

echo "1. Server Process Status:"
ps aux | grep -E "(node|java|python)" | grep -v grep

echo -e "\n2. Port Status:"
netstat -tlnp | grep -E "(3005|8000)"

echo -e "\n3. File System Status:"
ls -la /home/aspuser/app/volume/DISK01/TESTLIB/ | grep -E "(MSGSample|SAMDATA|MSGSAMP1)"

echo -e "\n4. Memory Usage:"
free -h

echo -e "\n5. Disk Usage:"
df -h /home/aspuser/app/volume

echo -e "\n6. Java Version:"
java -version 2>&1

echo -e "\n7. Catalog Verification:"
if grep -q "MSGSample" /home/aspuser/app/asp-manager/public/config/catalog.json; then
    echo "✓ MSGSample found in catalog"
else
    echo "✗ MSGSample NOT found in catalog"
fi

echo -e "\n=== Health Check Complete ==="
```

### Quick Test Execution
```bash
#!/bin/bash
# quick-test.sh

echo "=== MSGSample Quick Test ==="
echo "Testing CALL functionality..."

cd /home/aspuser/app/server/system-cmds
python3 -c "
from functions.call import CALL
result = CALL('CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01')
print(f'Test Result: {'PASS' if result else 'FAIL'}')
"

echo "=== Quick Test Complete ==="
```

### Log Analysis Tool
```bash
#!/bin/bash
# log-analysis.sh

echo "=== Log Analysis for MSGSample Issues ==="

echo "1. Recent Server Logs:"
tail -20 /home/aspuser/app/asp-manager/server/server.log

echo -e "\n2. System Errors:"
tail -20 /var/log/syslog | grep -i error

echo -e "\n3. Java Errors:"
ps aux | grep java | awk '{print $2}' | xargs -I {} cat /proc/{}/fd/2 2>/dev/null | tail -10

echo "=== Log Analysis Complete ==="
```

## Prevention Best Practices

### Regular Maintenance
1. **Weekly Tasks**:
   - Verify catalog.json integrity
   - Check file system permissions
   - Monitor disk space usage
   - Review error logs

2. **Monthly Tasks**:
   - Update Java runtime if needed
   - Backup configuration files
   - Performance benchmarking
   - Security assessment

### Monitoring Setup
1. **File System Monitoring**:
   ```bash
   # Add to crontab
   0 */6 * * * /usr/bin/find /home/aspuser/app/volume -name "*.jar" -type f -exec test -r {} \; || echo "JAR file access issue detected" | mail -s "ASP Alert" admin@company.com
   ```

2. **Performance Monitoring**:
   ```bash
   # Monitor execution times
   0 */2 * * * /home/aspuser/scripts/performance-check.sh
   ```

### Backup Strategy
1. **Critical Files to Backup**:
   - `/home/aspuser/app/asp-manager/public/config/catalog.json`
   - `/home/aspuser/app/volume/DISK01/TESTLIB/MSGSample.jar`
   - `/home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA`
   - `/home/aspuser/app/volume/DISK01/TESTLIB/MSGSAMP1`

2. **Backup Command**:
   ```bash
   tar -czf asp-backup-$(date +%Y%m%d).tar.gz \
     /home/aspuser/app/asp-manager/public/config/catalog.json \
     /home/aspuser/app/volume/DISK01/TESTLIB/
   ```

## Emergency Procedures

### System Recovery
1. **Complete System Reset**:
   ```bash
   # Stop services
   pkill -f "node.*asp-manager"
   pkill -f "java.*MSGSample"
   
   # Clear temporary files
   rm -f /tmp/asp-*
   
   # Restart services
   cd /home/aspuser/app/asp-manager
   npm start
   ```

2. **Data Recovery**:
   ```bash
   # Restore from backup
   tar -xzf asp-backup-YYYYMMDD.tar.gz -C /
   
   # Verify restoration
   java -jar /home/aspuser/app/volume/DISK01/TESTLIB/MSGSample.jar
   ```

### Escalation Contacts
- **System Administrator**: admin@company.com
- **Development Team**: dev-team@company.com  
- **QA Director**: qa-director@company.com
- **Emergency Hotline**: +1-555-ASP-HELP

---

**Document Version**: 1.0  
**Last Updated**: 2025-07-28  
**Prepared By**: QA Director  
**Next Review**: 2025-08-28