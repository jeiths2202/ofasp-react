/**
 * UI Call Tree Test
 * UI에서 COBOL Call Tree가 제대로 동작하는지 테스트
 */

const puppeteer = require('puppeteer');
const fs = require('fs');
const path = require('path');

async function testUICallTree() {
  console.log('🚀 Starting UI COBOL Call Tree Test');
  
  const browser = await puppeteer.launch({ 
    headless: true, 
    args: ['--no-sandbox', '--disable-setuid-sandbox'] 
  });
  
  try {
    const page = await browser.newPage();
    
    // Enable console logging
    page.on('console', msg => {
      const text = msg.text();
      if (text.includes('🔍') || text.includes('✅') || text.includes('❌') || 
          text.includes('Converting node') || text.includes('Found') || 
          text.includes('Analysis complete')) {
        console.log('BROWSER:', text);
      }
    });
    
    console.log('📄 Navigating to http://localhost:3005');
    await page.goto('http://localhost:3005', { waitUntil: 'networkidle0' });
    
    // Wait for page to load
    await page.waitForSelector('nav', { timeout: 10000 });
    console.log('✅ Page loaded successfully');
    
    // Navigate to AI Transform page
    console.log('🔄 Navigating to AI Transform page');
    await page.click('a[href="/transform"]');
    await page.waitForSelector('h1', { timeout: 5000 });
    
    // Check if the page has the correct title
    const title = await page.$eval('h1', el => el.textContent);
    console.log('📋 Page title:', title);
    
    // Wait a moment for the page to fully load
    await page.waitForTimeout(2000);
    
    // Check if file upload section exists
    const hasFileUpload = await page.$('input[type="file"]') !== null;
    console.log('📁 File upload section exists:', hasFileUpload);
    
    if (hasFileUpload) {
      console.log('✅ AI Transform page loaded correctly');
      
      // Try to simulate BACKUP00 selection if possible
      const backup00Button = await page.$('text=BACKUP00');
      if (backup00Button) {
        console.log('🔍 Found BACKUP00 button, clicking...');
        await backup00Button.click();
        
        // Wait for call tree analysis
        await page.waitForTimeout(3000);
        
        // Check for call tree content
        const callTreeContent = await page.$('.call-tree, [class*="call"], [class*="tree"]');
        console.log('🌳 Call tree content found:', callTreeContent !== null);
      } else {
        console.log('❌ BACKUP00 button not found - need to upload files first');
      }
    }
    
    console.log('✅ UI test completed successfully');
    
  } catch (error) {
    console.error('❌ UI test failed:', error.message);
  } finally {
    await browser.close();
  }
}

// Check if puppeteer is available
async function checkPuppeteer() {
  try {
    require('puppeteer');
    return true;
  } catch (e) {
    console.log('📦 Installing puppeteer...');
    const { execSync } = require('child_process');
    try {
      execSync('npm install puppeteer', { stdio: 'inherit' });
      return true;
    } catch (installError) {
      console.log('❌ Could not install puppeteer, running basic checks instead');
      return false;
    }
  }
}

async function runBasicTests() {
  console.log('🧪 Running basic COBOL Call Tree tests without UI');
  
  // Test the analyzer directly
  const CobolCallTreeAnalyzer = require('./src/utils/cobolCallTreeAnalyzer.ts');
  const analyzer = new CobolCallTreeAnalyzer();
  
  // Add some test programs
  const testData = {
    'BACKUP00': `PGM (BACKUP00)
CALL ASPAC0X.PRODLIB
CALL ASPAC03.DEVLIB
RETURN`,
    'ASPAC0X': `IDENTIFICATION DIVISION.
PROGRAM-ID. ASPAC0X.
PROCEDURE DIVISION.
CALL 'ASPAX01X'.
CALL 'ASPAX02X'.
STOP RUN.`,
    'ASPAX01X': `IDENTIFICATION DIVISION.
PROGRAM-ID. ASPAX01X.
PROCEDURE DIVISION.
DISPLAY 'ASPAX01X'.
STOP RUN.`
  };
  
  console.log('📥 Adding test programs to analyzer...');
  Object.entries(testData).forEach(([name, content]) => {
    analyzer.addProgram(name, content, name.includes('BACKUP') ? 'CL' : 'COBOL');
    console.log(`  ✅ Added ${name}`);
  });
  
  console.log('🌳 Performing call tree analysis...');
  const result = analyzer.analyzeCallTree();
  
  console.log('📊 Analysis Results:');
  console.log(`  - Programs: ${Object.keys(testData).length}`);
  console.log(`  - Root nodes: ${result.rootNodes.length}`);
  console.log(`  - Total calls: ${result.allCalls.length}`);
  console.log(`  - Missing programs: ${result.missingPrograms.length}`);
  
  // Check if BACKUP00 tree exists
  const backup00Node = result.rootNodes.find(node => node.name === 'BACKUP00');
  if (backup00Node) {
    console.log('✅ BACKUP00 root node found');
    console.log(`  - Children: ${backup00Node.children.length}`);
    console.log(`  - Calls: ${backup00Node.calls.length}`);
  } else {
    console.log('❌ BACKUP00 root node not found');
  }
  
  console.log('✅ Basic analyzer test completed');
}

// Main execution
async function main() {
  const hasPuppeteer = await checkPuppeteer();
  
  if (hasPuppeteer) {
    await testUICallTree();
  } else {
    await runBasicTests();
  }
}

if (require.main === module) {
  main().catch(console.error);
}

module.exports = { testUICallTree, runBasicTests };