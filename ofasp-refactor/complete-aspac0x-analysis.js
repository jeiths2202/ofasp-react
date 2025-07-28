/**
 * Complete ASPAC0X Analysis
 * ASPCOMMX를 포함한 완전한 분석
 */

const CobolCallTreeAnalyzer = require('./cobol-call-tree-analyzer.js');
const fs = require('fs');
const path = require('path');

function findAllConnectedPrograms(baseDir, startProgram = 'ASPAC0X') {
  const foundPrograms = new Map(); // program -> file content
  const toProcess = [startProgram];
  const processed = new Set();
  
  console.log('🔍 Finding all connected programs...\n');
  
  while (toProcess.length > 0) {
    const currentProgram = toProcess.shift();
    if (processed.has(currentProgram)) continue;
    
    processed.add(currentProgram);
    const programPath = path.join(baseDir, currentProgram);
    
    if (fs.existsSync(programPath)) {
      try {
        const content = fs.readFileSync(programPath, 'utf8');
        foundPrograms.set(currentProgram, content);
        console.log(`✅ Found: ${currentProgram} (${content.length} chars)`);
        
        // Extract CALL statements
        const callMatches = content.match(/CALL\s+['"]([A-Z0-9\-_X]+)['"]/gi);
        if (callMatches) {
          console.log(`  → Makes ${callMatches.length} CALL(s):`);
          callMatches.forEach(match => {
            const programMatch = match.match(/CALL\s+['"]([A-Z0-9\-_X]+)['"]/i);
            if (programMatch) {
              const calledProgram = programMatch[1];
              console.log(`    → CALL '${calledProgram}'`);
              if (!processed.has(calledProgram) && !toProcess.includes(calledProgram)) {
                toProcess.push(calledProgram);
              }
            }
          });
        } else {
          console.log(`  → No CALL statements found`);
        }
        console.log('');
      } catch (error) {
        console.log(`❌ Error reading ${currentProgram}: ${error.message}\n`);
      }
    } else {
      console.log(`❌ Not found: ${currentProgram}\n`);
    }
  }
  
  return foundPrograms;
}

function performCompleteAnalysis() {
  console.log('🧪 Complete ASPAC0X Analysis\n');
  
  const baseDir = '/data/assets/PRODLIB';
  
  // Step 1: Find all connected programs
  const allPrograms = findAllConnectedPrograms(baseDir, 'ASPAC0X');
  
  console.log(`📊 Discovery Summary: Found ${allPrograms.size} programs\n`);
  
  // Step 2: Load all programs into analyzer
  const analyzer = new CobolCallTreeAnalyzer();
  
  console.log('📥 Loading programs into Call Tree Analyzer:\n');
  for (const [programName, content] of allPrograms) {
    analyzer.addProgram(programName, content, 'COBOL');
    console.log(`  ✅ Loaded: ${programName}`);
  }
  
  // Step 3: Perform call tree analysis
  console.log('\n🌳 Performing Call Tree Analysis...\n');
  const result = analyzer.analyzeCallTree();
  
  console.log('📊 Analysis Results:');
  console.log(`  - Programs analyzed: ${analyzer.programs.size}`);
  console.log(`  - Root programs: ${result.rootNodes.length}`);
  console.log(`  - Total CALL relationships: ${result.allCalls.length}`);
  console.log(`  - Missing programs: ${result.missingPrograms.length}`);
  console.log(`  - Cyclic references: ${result.cyclicReferences.length}`);
  
  if (result.missingPrograms.length > 0) {
    console.log(`\n❌ Missing Programs: ${result.missingPrograms.join(', ')}`);
  }
  
  // Step 4: Show all CALL relationships
  console.log('\n🔗 All CALL Relationships:');
  result.allCalls.forEach((call, index) => {
    const status = result.missingPrograms.includes(call.calleeProgram) ? '❌' : '✅';
    console.log(`  ${(index + 1).toString().padStart(2)}: ${status} ${call.callerProgram} → ${call.calleeProgram} (line: ${call.lineNumber})`);
  });
  
  // Step 5: Print complete tree
  console.log('\n📋 Complete Call Tree Structure:');
  const treeOutput = analyzer.printCallTree(result);
  console.log(treeOutput);
  
  // Step 6: Focus on ASPAC0X tree
  const aspac0xRoot = result.rootNodes.find(node => node.name === 'ASPAC0X');
  if (aspac0xRoot) {
    console.log('🌲 ASPAC0X Complete Call Tree:');
    printDetailedTree(aspac0xRoot, '', true, 0, result.missingPrograms);
  }
  
  // Step 7: Tree statistics
  console.log('\n📈 Call Tree Statistics:');
  const stats = calculateTreeStatistics(result.rootNodes);
  console.log(`  - Maximum depth: ${stats.maxDepth}`);
  console.log(`  - Total nodes in tree: ${stats.totalNodes}`);
  console.log(`  - Leaf nodes: ${stats.leafNodes}`);
  console.log(`  - Available programs: ${stats.availableNodes}`);
  console.log(`  - Missing programs: ${stats.missingNodes}`);
  
  return result;
}

function printDetailedTree(node, prefix = '', isLast = true, depth = 0, missingPrograms = []) {
  const connector = isLast ? '└── ' : '├── ';
  const status = missingPrograms.includes(node.name) ? '❌' : '✅';
  const indent = depth === 0 ? '' : prefix;
  
  console.log(`${indent}${connector}${status} ${node.name}`);
  
  if (node.children && node.children.length > 0 && depth < 10) {
    const newPrefix = prefix + (isLast ? '    ' : '│   ');
    node.children.forEach((child, index) => {
      const childIsLast = index === node.children.length - 1;
      printDetailedTree(child, newPrefix, childIsLast, depth + 1, missingPrograms);
    });
  }
}

function calculateTreeStatistics(rootNodes) {
  let maxDepth = 0;
  let totalNodes = 0;
  let leafNodes = 0;
  let availableNodes = 0;
  let missingNodes = 0;
  
  function traverseNode(node, depth = 0) {
    totalNodes++;
    maxDepth = Math.max(maxDepth, depth);
    
    if (node.isFound !== false) { // Consider undefined as found
      availableNodes++;
    } else {
      missingNodes++;
    }
    
    if (!node.children || node.children.length === 0) {
      leafNodes++;
    } else {
      node.children.forEach(child => traverseNode(child, depth + 1));
    }
  }
  
  rootNodes.forEach(root => traverseNode(root));
  
  return {
    maxDepth,
    totalNodes,
    leafNodes,
    availableNodes,
    missingNodes
  };
}

// Main execution
if (require.main === module) {
  performCompleteAnalysis();
}

module.exports = { performCompleteAnalysis };