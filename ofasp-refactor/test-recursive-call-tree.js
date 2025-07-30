/**
 * Recursive Call Tree Analysis
 * ASPAC0X와 모든 관련 프로그램들의 재귀적 Call Tree 분석
 */

const CobolCallTreeAnalyzer = require('./cobol-call-tree-analyzer.js');
const fs = require('fs');
const path = require('path');

function findAllRelatedPrograms(baseDir, startingPrograms = ['ASPAC0X']) {
  const foundPrograms = new Set();
  const toProcess = [...startingPrograms];
  const processed = new Set();
  
  console.log('🔍 Finding all related programs recursively...\n');
  
  while (toProcess.length > 0) {
    const currentProgram = toProcess.pop();
    if (processed.has(currentProgram)) continue;
    
    processed.add(currentProgram);
    const filePath = path.join(baseDir, currentProgram);
    
    if (fs.existsSync(filePath)) {
      foundPrograms.add(currentProgram);
      console.log(`✅ Found: ${currentProgram}`);
      
      try {
        const content = fs.readFileSync(filePath, 'utf8');
        
        // Extract CALL statements to find more programs to process
        const callMatches = content.match(/CALL\s+['"]([A-Z0-9\-_X]+)['"]/gi);
        if (callMatches) {
          callMatches.forEach(match => {
            const programMatch = match.match(/CALL\s+['"]([A-Z0-9\-_X]+)['"]/i);
            if (programMatch) {
              const calledProgram = programMatch[1];
              if (!processed.has(calledProgram) && !toProcess.includes(calledProgram)) {
                console.log(`  → Found CALL to: ${calledProgram}`);
                toProcess.push(calledProgram);
              }
            }
          });
        }
      } catch (error) {
        console.log(`❌ Error reading ${currentProgram}: ${error.message}`);
      }
    } else {
      console.log(`❌ Not found: ${currentProgram}`);
    }
  }
  
  console.log(`\n📊 Total programs found: ${foundPrograms.size}`);
  return Array.from(foundPrograms);
}

function loadAllPrograms(baseDir, programList) {
  const analyzer = new CobolCallTreeAnalyzer();
  
  console.log('\n📥 Loading all programs into analyzer...\n');
  
  programList.forEach(programName => {
    const filePath = path.join(baseDir, programName);
    try {
      const content = fs.readFileSync(filePath, 'utf8');
      analyzer.addProgram(programName, content, 'COBOL');
      
      // Show CALL statements for each program
      const lines = content.split('\n');
      const callLines = lines.filter(line => line.trim().includes('CALL '));
      if (callLines.length > 0) {
        console.log(`📄 ${programName}:`);
        callLines.forEach(line => {
          const lineNum = lines.indexOf(line) + 1;
          const trimmed = line.trim();
          console.log(`  Line ${lineNum}: ${trimmed}`);
        });
      } else {
        console.log(`📄 ${programName}: No CALL statements`);
      }
    } catch (error) {
      console.log(`❌ Error loading ${programName}: ${error.message}`);
    }
  });
  
  return analyzer;
}

function analyzeRecursiveCallTree() {
  console.log('🧪 Recursive Call Tree Analysis for ASPAC0X\n');
  
  const baseDir = '/data/assets/PRODLIB';
  
  // Step 1: Find all related programs recursively
  const allPrograms = findAllRelatedPrograms(baseDir, ['ASPAC0X']);
  
  // Step 2: Load all programs into analyzer
  const analyzer = loadAllPrograms(baseDir, allPrograms);
  
  // Step 3: Perform complete call tree analysis
  console.log('\n🌳 Performing Complete Call Tree Analysis...\n');
  const result = analyzer.analyzeCallTree();
  
  console.log('📊 Complete Analysis Summary:');
  console.log(`  - Programs loaded: ${analyzer.programs.size}`);
  console.log(`  - Root nodes: ${result.rootNodes.length}`);
  console.log(`  - Total calls: ${result.allCalls.length}`);
  console.log(`  - Missing programs: ${result.missingPrograms.length}`);
  console.log(`  - Cyclic references: ${result.cyclicReferences.length}`);
  
  if (result.missingPrograms.length > 0) {
    console.log(`\n❌ Missing programs: ${result.missingPrograms.join(', ')}`);
  }
  
  if (result.cyclicReferences.length > 0) {
    console.log(`\n🔄 Cyclic references detected: ${result.cyclicReferences.length}`);
  }
  
  // Step 4: Show detailed call information
  console.log('\n🔍 Complete Call Relationships:');
  result.allCalls.forEach(call => {
    const status = result.missingPrograms.includes(call.calleeProgram) ? '❌' : '✅';
    console.log(`${status} ${call.callerProgram} → ${call.calleeProgram} (line: ${call.lineNumber})`);
  });
  
  // Step 5: Print complete tree structure
  console.log('\n📋 Complete Recursive Call Tree:');
  const treeOutput = analyzer.printCallTree(result);
  console.log(treeOutput);
  
  // Step 6: Enhanced tree visualization
  console.log('🌳 Enhanced Tree Visualization:');
  result.rootNodes.forEach(rootNode => {
    console.log(`\n🌲 Tree starting from: ${rootNode.name}`);
    printEnhancedTree(rootNode, '', true, 0);
  });
  
  // Step 7: Statistics by depth
  console.log('\n📈 Call Tree Statistics:');
  const stats = calculateTreeStats(result.rootNodes);
  console.log(`  - Maximum depth: ${stats.maxDepth}`);
  console.log(`  - Average depth: ${stats.avgDepth.toFixed(2)}`);
  console.log(`  - Total nodes: ${stats.totalNodes}`);
  console.log(`  - Leaf nodes: ${stats.leafNodes}`);
  
  return result;
}

function printEnhancedTree(node, prefix = '', isLast = true, depth = 0) {
  const connector = isLast ? '└── ' : '├── ';
  const status = node.isFound ? '✅' : '❌';
  const typeIcon = node.type === 'CL' ? '🔧' : '📄';
  const cyclicMark = node.cyclic ? ' 🔄' : '';
  
  console.log(`${prefix}${connector}${status}${typeIcon} ${node.name}${cyclicMark}`);
  
  if (node.children && node.children.length > 0 && depth < 10) { // Prevent infinite recursion
    const newPrefix = prefix + (isLast ? '    ' : '│   ');
    node.children.forEach((child, index) => {
      const childIsLast = index === node.children.length - 1;
      printEnhancedTree(child, newPrefix, childIsLast, depth + 1);
    });
  }
}

function calculateTreeStats(rootNodes) {
  let maxDepth = 0;
  let totalDepth = 0;
  let totalNodes = 0;
  let leafNodes = 0;
  
  function traverseNode(node, depth = 0) {
    totalNodes++;
    totalDepth += depth;
    maxDepth = Math.max(maxDepth, depth);
    
    if (!node.children || node.children.length === 0) {
      leafNodes++;
    } else {
      node.children.forEach(child => traverseNode(child, depth + 1));
    }
  }
  
  rootNodes.forEach(root => traverseNode(root));
  
  return {
    maxDepth,
    avgDepth: totalNodes > 0 ? totalDepth / totalNodes : 0,
    totalNodes,
    leafNodes
  };
}

// Main execution
if (require.main === module) {
  analyzeRecursiveCallTree();
}

module.exports = { analyzeRecursiveCallTree };