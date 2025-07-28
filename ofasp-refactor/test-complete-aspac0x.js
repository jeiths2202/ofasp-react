/**
 * Complete ASPAC0X Call Tree Analysis
 * ASPAC0X와 모든 관련 프로그램들의 완전한 Call Tree 분석
 */

const CobolCallTreeAnalyzer = require('./cobol-call-tree-analyzer.js');
const fs = require('fs');
const path = require('path');

function loadCompleteASPAC0XFiles() {
  const analyzer = new CobolCallTreeAnalyzer();
  const baseDir = '/data/assets/PRODLIB';
  
  // Complete ASPAC0X related files
  const files = [
    'ASPAC0X',    // Root program
    'ASPAX01X',   // Called by ASPAC0X
    'ASPAX02X',   // Called by ASPAC0X
    'ASPAX03X'    // Called by ASPAC0X
  ];
  
  console.log('🔍 Loading Complete ASPAC0X Call Tree Files\n');
  
  files.forEach(fileName => {
    const filePath = path.join(baseDir, fileName);
    try {
      if (fs.existsSync(filePath)) {
        const content = fs.readFileSync(filePath, 'utf8');
        console.log(`✅ Loaded: ${fileName} (${content.length} chars)`);
        analyzer.addProgram(fileName, content, 'COBOL');
        
        // Show CALL statements for each file
        const lines = content.split('\n');
        const callLines = lines.filter(line => line.trim().includes('CALL '));
        if (callLines.length > 0) {
          console.log(`   CALL statements found:`);
          callLines.forEach(line => {
            const lineNum = lines.indexOf(line) + 1;
            console.log(`     Line ${lineNum}: ${line.trim()}`);
          });
        } else {
          console.log(`   No CALL statements found`);
        }
        console.log('');
      } else {
        console.log(`❌ Not found: ${fileName}`);
      }
    } catch (error) {
      console.log(`❌ Error loading ${fileName}: ${error.message}`);
    }
  });
  
  return analyzer;
}

function analyzeCompleteCallTree() {
  console.log('🧪 Complete ASPAC0X Call Tree Analysis\n');
  
  try {
    // Load all related files
    const analyzer = loadCompleteASPAC0XFiles();
    
    // Analyze call tree
    console.log('🌳 Performing Call Tree Analysis...\n');
    const result = analyzer.analyzeCallTree();
    
    console.log(`📊 Analysis Summary:`);
    console.log(`  - Root nodes: ${result.rootNodes.length}`);
    console.log(`  - Total calls: ${result.allCalls.length}`);
    console.log(`  - Missing programs: ${result.missingPrograms.length}`);
    console.log(`  - Programs loaded: ${analyzer.programs.size}`);
    
    if (result.missingPrograms.length > 0) {
      console.log(`\n❌ Missing programs: ${result.missingPrograms.join(', ')}`);
    }
    
    console.log('\n🔍 Detailed Call Information:');
    result.allCalls.forEach(call => {
      const status = result.missingPrograms.includes(call.calleeProgram) ? '❌' : '✅';
      console.log(`${status} ${call.callerProgram} -> ${call.calleeProgram} (line: ${call.lineNumber})`);
    });
    
    // Print complete tree structure
    console.log('\n📋 Complete Call Tree Structure:');
    const treeOutput = analyzer.printCallTree(result);
    console.log(treeOutput);
    
    // Focus on ASPAC0X specifically
    console.log('✨ ASPAC0X Specific Analysis:');
    const aspac0xRoot = result.rootNodes.find(node => node.name === 'ASPAC0X');
    if (aspac0xRoot) {
      console.log('\n🌳 ASPAC0X Complete Call Tree:');
      console.log(analyzer.printNode(aspac0xRoot, 0));
      
      console.log('\n📈 Call Tree Structure Visualization:');
      console.log('ASPAC0X');
      aspac0xRoot.children.forEach((child, index) => {
        const isLast = index === aspac0xRoot.children.length - 1;
        const connector = isLast ? '└──' : '├──';
        const status = child.isFound ? '✅' : '❌';
        console.log(`${connector} ${status} ${child.name}`);
        
        // Show grandchildren if any
        if (child.children && child.children.length > 0) {
          child.children.forEach((grandchild, gIndex) => {
            const prefix = isLast ? '   ' : '│  ';
            const gIsLast = gIndex === child.children.length - 1;
            const gConnector = gIsLast ? '└──' : '├──';
            const gStatus = grandchild.isFound ? '✅' : '❌';
            console.log(`${prefix}${gConnector} ${gStatus} ${grandchild.name}`);
          });
        }
      });
    } else {
      console.log('\n⚠️ ASPAC0X is not a root node');
    }
    
    console.log('\n🎯 Expected vs Actual Results:');
    console.log('Expected ASPAC0X calls: ASPAX01X, ASPAX02X, ASPAX03X');
    const aspac0xCalls = result.allCalls.filter(call => call.callerProgram === 'ASPAC0X');
    console.log(`Actual ASPAC0X calls: ${aspac0xCalls.map(c => c.calleeProgram).join(', ')}`);
    
    console.log('\nExpected tree structure:');
    console.log('ASPAC0X');
    console.log('├── ASPAX01X');
    console.log('│   └── ASPCOMMX (missing)');
    console.log('├── ASPAX02X');
    console.log('│   └── ASPCOMMX (missing)');
    console.log('└── ASPAX03X');
    console.log('    └── ASPCOMMX (missing)');
    
    console.log('\n✅ Complete analysis finished successfully!');
    return result;
    
  } catch (error) {
    console.error('❌ Analysis failed:', error);
    console.error(error.stack);
    process.exit(1);
  }
}

// Main execution
if (require.main === module) {
  analyzeCompleteCallTree();
}

module.exports = { analyzeCompleteCallTree };