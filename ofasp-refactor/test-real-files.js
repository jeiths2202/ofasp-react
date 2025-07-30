/**
 * Real COBOL Files Call Tree Test
 * /data/assets/PRODLIB의 실제 파일들로 테스트
 */

const CobolCallTreeAnalyzer = require('./cobol-call-tree-analyzer.js');
const fs = require('fs');
const path = require('path');

function loadRealFiles() {
  const analyzer = new CobolCallTreeAnalyzer();
  const baseDir = '/data/assets/PRODLIB';
  
  // Test files to load
  const testFiles = [
    'ASPAC0X',
    'ASPAC01X', 
    'ASPAC02X',
    'ASPAC03X',
    'ASPAC00',
    'ASPAC01',
    'ASPAC02',
    'ASPAC03'
  ];
  
  console.log('🔍 Loading real COBOL files from /data/assets/PRODLIB/\n');
  
  testFiles.forEach(fileName => {
    const filePath = path.join(baseDir, fileName);
    try {
      if (fs.existsSync(filePath)) {
        const content = fs.readFileSync(filePath, 'utf8');
        console.log(`✅ Loaded: ${fileName} (${content.length} chars)`);
        analyzer.addProgram(fileName, content, 'COBOL');
      } else {
        console.log(`❌ Not found: ${fileName}`);
      }
    } catch (error) {
      console.log(`❌ Error loading ${fileName}: ${error.message}`);
    }
  });
  
  return analyzer;
}

function runRealFileTest() {
  console.log('🧪 Starting Real COBOL File Call Tree Test\n');
  
  try {
    // Load real files
    const analyzer = loadRealFiles();
    
    console.log('\n📊 Debug Information:');
    analyzer.printDebugInfo();
    
    // Analyze call tree
    console.log('\n🌳 Call Tree Analysis:');
    const result = analyzer.analyzeCallTree();
    
    console.log(`Root nodes: ${result.rootNodes.length}`);
    console.log(`Total calls: ${result.allCalls.length}`);
    console.log(`Missing programs: ${result.missingPrograms.length}`);
    
    if (result.missingPrograms.length > 0) {
      console.log('Missing programs:', result.missingPrograms);
    }
    
    // Print tree structure
    console.log('\n📋 Tree Structure Output:');
    const treeOutput = analyzer.printCallTree(result);
    console.log(treeOutput);
    
    // Show individual call information
    console.log('🔍 Individual Call Information:');
    result.allCalls.forEach(call => {
      const status = result.missingPrograms.includes(call.calleeProgram) ? '❌' : '✅';
      console.log(`${status} ${call.callerProgram} -> ${call.calleeProgram} (line: ${call.lineNumber})`);
    });
    
    // Specific test for ASPAC0X
    console.log('\n✨ ASPAC0X Specific Test:');
    const aspac0xCalls = result.allCalls.filter(call => call.callerProgram === 'ASPAC0X');
    console.log(`ASPAC0X makes ${aspac0xCalls.length} calls:`);
    aspac0xCalls.forEach(call => {
      console.log(`  -> ${call.calleeProgram} (line: ${call.lineNumber})`);
    });
    
    // Find ASPAC0X in root nodes
    const aspac0xRoot = result.rootNodes.find(node => node.name === 'ASPAC0X');
    if (aspac0xRoot) {
      console.log('\n🌳 ASPAC0X Call Tree:');
      console.log(analyzer.printNode(aspac0xRoot, 0));
    } else {
      console.log('\n⚠️ ASPAC0X is not a root node (might be called by other programs)');
    }
    
    console.log('\n✅ Real file test completed successfully!');
    return result;
    
  } catch (error) {
    console.error('❌ Test failed:', error);
    console.error(error.stack);
    process.exit(1);
  }
}

// Main execution
if (require.main === module) {
  runRealFileTest();
}

module.exports = { runRealFileTest };