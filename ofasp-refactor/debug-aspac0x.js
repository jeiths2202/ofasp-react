/**
 * ASPAC0X Debug Analysis
 * 정확한 파싱 과정을 단계별로 디버그
 */

const CobolCallTreeAnalyzer = require('./cobol-call-tree-analyzer.js');
const fs = require('fs');

function debugASPAC0X() {
  console.log('🔍 ASPAC0X Debug Analysis\n');
  
  // Load file
  const filePath = '/data/assets/PRODLIB/ASPAC0X';
  const content = fs.readFileSync(filePath, 'utf8');
  
  console.log('📄 File Information:');
  console.log(`File size: ${content.length} characters`);
  console.log(`Line count: ${content.split('\n').length}`);
  
  // Show each line with line numbers
  console.log('\n📝 File Content (line by line):');
  const lines = content.split('\n');
  lines.forEach((line, index) => {
    const lineNum = index + 1;
    if (line.trim().includes('CALL') || lineNum >= 20 && lineNum <= 26) {
      console.log(`${lineNum.toString().padStart(2, '0')}: "${line}"`);
      
      // Show hex for CALL lines
      if (line.trim().includes('CALL')) {
        const hex = Buffer.from(line, 'utf8').toString('hex');
        console.log(`    HEX: ${hex}`);
        
        // Show each character
        console.log(`    CHARS: ${line.split('').map(c => {
          const code = c.charCodeAt(0);
          if (code >= 32 && code <= 126) {
            return `'${c}'`;
          } else {
            return `\\x${code.toString(16).padStart(2, '0')}`;
          }
        }).join(' ')}`);
      }
    }
  });
  
  // Test regex patterns manually
  console.log('\n🔍 Manual CALL Pattern Testing:');
  const callPatterns = [
    /CALL\s+['"]([A-Z0-9\-_]+)['"](?:\s+USING.*?)?(?:\s+RETURNING.*?)?/gi,
    /CALL\s+([A-Z0-9\-_]+)(?:\s+USING.*?)?(?:\s+RETURNING.*?)?/gi
  ];
  
  callPatterns.forEach((pattern, index) => {
    console.log(`\nPattern ${index + 1}: ${pattern}`);
    const matches = content.match(pattern);
    if (matches) {
      console.log(`Found ${matches.length} matches:`);
      matches.forEach(match => {
        console.log(`  - "${match}"`);
      });
    } else {
      console.log('No matches found');
    }
  });
  
  // Test with analyzer
  console.log('\n🧪 Analyzer Testing:');
  const analyzer = new CobolCallTreeAnalyzer();
  analyzer.addProgram('ASPAC0X', content, 'COBOL');
  
  // Get calls using analyzer method
  console.log('\nCalls found by analyzer:');
  const calls = analyzer.getCallsForProgram('ASPAC0X');
  calls.forEach(call => {
    console.log(`  - ${call.callerProgram} -> ${call.calleeProgram} (line: ${call.lineNumber})`);
    console.log(`    Statement: "${call.callStatement}"`);
  });
  
  // Test line by line
  console.log('\n🔬 Line-by-Line Analysis:');
  lines.forEach((line, index) => {
    const trimmed = line.trim();
    if (trimmed.includes('CALL')) {
      const lineNum = index + 1;
      console.log(`\nLine ${lineNum}: "${trimmed}"`);
      
      // Test each pattern individually
      callPatterns.forEach((pattern, pIndex) => {
        const regex = new RegExp(pattern.source, pattern.flags);
        const match = regex.exec(trimmed);
        if (match) {
          console.log(`  Pattern ${pIndex + 1} matched: "${match[0]}" -> program: "${match[1]}"`);
        } else {
          console.log(`  Pattern ${pIndex + 1}: No match`);
        }
      });
    }
  });
  
  console.log('\n✅ Debug analysis complete');
}

// Main execution
if (require.main === module) {
  debugASPAC0X();
}

module.exports = { debugASPAC0X };