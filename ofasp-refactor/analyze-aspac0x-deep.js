/**
 * Deep Analysis of ASPAC0X
 * 바이트 레벨 분석 및 실제 프로그램 존재 확인
 */

const CobolCallTreeAnalyzer = require('./cobol-call-tree-analyzer.js');
const fs = require('fs');
const path = require('path');

function analyzeASPAC0XDeep() {
  console.log('🔍 Deep Analysis of ASPAC0X\n');
  
  const filePath = '/data/assets/PRODLIB/ASPAC0X';
  const baseDir = '/data/assets/PRODLIB';
  
  // 1. File basic information
  const stats = fs.statSync(filePath);
  const content = fs.readFileSync(filePath, 'utf8');
  
  console.log('📄 File Basic Information:');
  console.log(`  Path: ${filePath}`);
  console.log(`  Size: ${stats.size} bytes`);
  console.log(`  Modified: ${stats.mtime}`);
  console.log(`  Content length: ${content.length} characters`);
  console.log(`  Lines: ${content.split('\n').length}`);
  
  // 2. Detailed line analysis
  console.log('\n📝 Line-by-Line Analysis:');
  const lines = content.split('\n');
  lines.forEach((line, index) => {
    const lineNum = index + 1;
    if (lineNum >= 20 && lineNum <= 26) {
      console.log(`${lineNum.toString().padStart(2)}: "${line}"`);
      if (line.includes('CALL')) {
        console.log(`    → This line contains a CALL statement`);
        console.log(`    → Length: ${line.length} chars`);
        console.log(`    → Trimmed: "${line.trim()}"`);
      }
    }
  });
  
  // 3. Extract CALL statements manually
  console.log('\n🔍 Manual CALL Statement Extraction:');
  const callStatements = [];
  lines.forEach((line, index) => {
    const trimmed = line.trim();
    if (trimmed.includes('CALL')) {
      const lineNum = index + 1;
      // Try different CALL patterns
      const patterns = [
        /CALL\s+'([A-Z0-9\-_X]+)'/i,  // CALL 'PROGRAM'
        /CALL\s+"([A-Z0-9\-_X]+)"/i,  // CALL "PROGRAM"
        /CALL\s+([A-Z0-9\-_X]+)\s*\./i, // CALL PROGRAM.
        /CALL\s+([A-Z0-9\-_X]+)(?:\s|$)/i // CALL PROGRAM
      ];
      
      let found = false;
      patterns.forEach((pattern, pIndex) => {
        const match = pattern.exec(trimmed);
        if (match && !found) {
          callStatements.push({
            line: lineNum,
            statement: trimmed,
            program: match[1],
            pattern: pIndex + 1
          });
          console.log(`  Line ${lineNum}: CALL '${match[1]}' (Pattern ${pIndex + 1})`);
          found = true;
        }
      });
      
      if (!found) {
        console.log(`  Line ${lineNum}: CALL found but no pattern matched: "${trimmed}"`);
      }
    }
  });
  
  // 4. Check existence of called programs
  console.log('\n📂 Checking Existence of Called Programs:');
  const programsToCheck = callStatements.map(cs => cs.program);
  const existingPrograms = [];
  const missingPrograms = [];
  
  programsToCheck.forEach(program => {
    const programPath = path.join(baseDir, program);
    if (fs.existsSync(programPath)) {
      existingPrograms.push(program);
      console.log(`  ✅ ${program} - EXISTS`);
    } else {
      missingPrograms.push(program);
      console.log(`  ❌ ${program} - NOT FOUND`);
    }
  });
  
  // 5. Load and analyze existing programs
  console.log('\n🔄 Loading and Analyzing Called Programs:');
  const analyzer = new CobolCallTreeAnalyzer();
  
  // Add ASPAC0X first
  analyzer.addProgram('ASPAC0X', content, 'COBOL');
  console.log(`  📄 Loaded ASPAC0X`);
  
  // Add existing called programs
  existingPrograms.forEach(program => {
    try {
      const programPath = path.join(baseDir, program);
      const programContent = fs.readFileSync(programPath, 'utf8');
      analyzer.addProgram(program, programContent, 'COBOL');
      console.log(`  📄 Loaded ${program} (${programContent.length} chars)`);
      
      // Show what this program calls
      const programLines = programContent.split('\n');
      const programCalls = programLines.filter(line => line.trim().includes('CALL'));
      if (programCalls.length > 0) {
        console.log(`    → ${program} makes ${programCalls.length} CALL(s):`);
        programCalls.forEach(callLine => {
          const callMatch = callLine.match(/CALL\s+['"]?([A-Z0-9\-_X]+)['"]?/i);
          if (callMatch) {
            console.log(`      → CALL '${callMatch[1]}'`);
          }
        });
      }
    } catch (error) {
      console.log(`  ❌ Error loading ${program}: ${error.message}`);
    }
  });
  
  // 6. Perform call tree analysis
  console.log('\n🌳 Call Tree Analysis:');
  const result = analyzer.analyzeCallTree();
  
  console.log(`  Programs loaded: ${analyzer.programs.size}`);
  console.log(`  Root nodes: ${result.rootNodes.length}`);
  console.log(`  Total calls: ${result.allCalls.length}`);
  console.log(`  Missing programs: ${result.missingPrograms.length}`);
  
  // 7. Show complete call tree
  console.log('\n📋 Complete Call Tree:');
  const treeOutput = analyzer.printCallTree(result);
  console.log(treeOutput);
  
  // 8. Summary
  console.log('📊 Analysis Summary:');
  console.log(`  ASPAC0X makes ${callStatements.length} CALL statements:`);
  callStatements.forEach(cs => {
    const status = existingPrograms.includes(cs.program) ? '✅' : '❌';
    console.log(`    ${status} Line ${cs.line}: CALL '${cs.program}'`);
  });
  
  console.log(`\n  Programs found: ${existingPrograms.length}/${programsToCheck.length}`);
  console.log(`  Missing programs: ${missingPrograms.join(', ') || 'None'}`);
  
  // 9. Recommended next steps
  if (missingPrograms.length > 0) {
    console.log('\n💡 Recommended Actions:');
    console.log('  - Search for missing programs in other directories');
    console.log('  - Check if programs have different naming conventions');
    console.log('  - Verify if programs are dynamically loaded');
  }
  
  return {
    callStatements,
    existingPrograms,
    missingPrograms,
    treeResult: result
  };
}

// Main execution
if (require.main === module) {
  analyzeASPAC0XDeep();
}

module.exports = { analyzeASPAC0XDeep };