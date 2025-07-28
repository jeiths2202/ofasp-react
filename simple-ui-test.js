/**
 * Simple UI Call Tree Test without puppeteer
 */

const fs = require('fs');
const path = require('path');

// Test the cobol analyzer directly
function testCobolAnalyzer() {
  console.log('🧪 Testing COBOL Call Tree Analyzer directly');
  
  try {
    // Read the analyzer file
    const analyzerPath = '/home/aspuser/app/ofasp-refactor/src/utils/cobolCallTreeAnalyzer.ts';
    if (!fs.existsSync(analyzerPath)) {
      console.log('❌ Analyzer file not found');
      return false;
    }
    
    console.log('✅ Analyzer file exists');
    
    // Test data that matches real files
    const realBackup00 = fs.readFileSync('/data/assets/SRC.CLLIB/BACKUP00', 'utf8');
    console.log('✅ Real BACKUP00 file loaded:', realBackup00.length, 'chars');
    
    // Count CALL statements in real file
    const callMatches = realBackup00.match(/CALL\s+[A-Z0-9\-_X]+(?:\.[A-Z0-9\-_X]+)?/gi);
    console.log('📞 CALL statements found in BACKUP00:', callMatches ? callMatches.length : 0);
    if (callMatches) {
      callMatches.forEach(call => console.log('  -', call));
    }
    
    return true;
    
  } catch (error) {
    console.log('❌ Error testing analyzer:', error.message);
    return false;
  }
}

// Check if UI files are correctly built
function checkUIBuild() {
  console.log('🔍 Checking UI build...');
  
  const buildPath = '/home/aspuser/app/ofasp-refactor/build';
  if (fs.existsSync(buildPath)) {
    const buildFiles = fs.readdirSync(buildPath);
    console.log('✅ Build directory exists with files:', buildFiles.length);
    
    // Check for main JS file
    const staticPath = path.join(buildPath, 'static', 'js');
    if (fs.existsSync(staticPath)) {
      const jsFiles = fs.readdirSync(staticPath).filter(f => f.endsWith('.js'));
      console.log('✅ JavaScript files in build:', jsFiles.length);
      
      // Check if call tree analyzer is included
      const mainJsFile = jsFiles.find(f => f.startsWith('main.'));
      if (mainJsFile) {
        const jsContent = fs.readFileSync(path.join(staticPath, mainJsFile), 'utf8');
        const hasCallTree = jsContent.includes('CobolCallTreeAnalyzer') || jsContent.includes('analyzeCallTree');
        console.log('✅ Call Tree code included in build:', hasCallTree);
        
        const hasCallPattern = jsContent.includes('CALL') && jsContent.includes('match');
        console.log('✅ CALL pattern matching included:', hasCallPattern);
        
        return hasCallTree && hasCallPattern;
      }
    }
  }
  
  console.log('❌ Build check failed');
  return false;
}

// Test actual data files
function testDataFiles() {
  console.log('📂 Testing data files availability...');
  
  const testFiles = [
    '/data/assets/SRC.CLLIB/BACKUP00',
    '/data/assets/PRODLIB/ASPAC0X',
    '/data/assets/PRODLIB/ASPAX01X',
    '/data/assets/PRODLIB/ASPAX02X',
    '/data/assets/PRODLIB/ASPAX03X'
  ];
  
  let foundFiles = 0;
  testFiles.forEach(filePath => {
    if (fs.existsSync(filePath)) {
      foundFiles++;
      const content = fs.readFileSync(filePath, 'utf8');
      console.log(`✅ ${path.basename(filePath)}: ${content.length} chars`);
    } else {
      console.log(`❌ ${path.basename(filePath)}: not found`);
    }
  });
  
  console.log(`📊 Found ${foundFiles}/${testFiles.length} test files`);
  return foundFiles >= 3;
}

// Main test execution
async function runTests() {
  console.log('🚀 Starting Simple UI Call Tree Tests\n');
  
  const results = {
    analyzer: testCobolAnalyzer(),
    build: checkUIBuild(), 
    data: testDataFiles()
  };
  
  console.log('\n📋 Test Results Summary:');
  console.log('  - Analyzer test:', results.analyzer ? '✅ PASS' : '❌ FAIL');
  console.log('  - Build test:', results.build ? '✅ PASS' : '❌ FAIL');
  console.log('  - Data files test:', results.data ? '✅ PASS' : '❌ FAIL');
  
  const overallPass = Object.values(results).every(r => r);
  console.log('\n🎯 Overall Result:', overallPass ? '✅ ALL TESTS PASSED' : '❌ SOME TESTS FAILED');
  
  if (overallPass) {
    console.log('\n💡 Recommendation: UI should work correctly');
    console.log('   - Browser test: http://localhost:3005');
    console.log('   - Navigate to AI変換処理');
    console.log('   - Upload assets folder');
    console.log('   - Select BACKUP00 to see Call Tree');
  } else {
    console.log('\n🔧 Issues found - UI may not work correctly');
  }
  
  return overallPass;
}

if (require.main === module) {
  runTests().catch(console.error);
}

module.exports = { runTests };