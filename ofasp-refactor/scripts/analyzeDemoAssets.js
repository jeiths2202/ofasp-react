#!/usr/bin/env node

/**
 * Demo Asset Analysis Script with Enhanced Features
 * Analyzes generated demo assets and produces comprehensive report
 */

const fs = require('fs');
const path = require('path');

// Import analyzers
const { CobolAnalyzer, CopybookAnalyzer } = require('./analyzeAssets');

// PL/I Analyzer
class PLIAnalyzer {
  constructor() {
    this.analyzedAssets = new Set();
  }
  
  analyze(filePath, content) {
    const lines = content.split('\n');
    const fileName = path.basename(filePath);
    
    const result = {
      fileName,
      filePath,
      fileType: 'PLI',
      encoding: 'ASCII',
      size: content.length,
      lineCount: lines.length,
      dependencies: {
        programs: [],
        copybooks: [],
        maps: [],
        files: [],
        sqlTables: []
      },
      missingAssets: [],
      unsupportedStatements: [],
      metadata: {
        procedureName: '',
        hasSQL: false,
        mainProgram: false
      },
      analysisDate: new Date().toISOString()
    };

    lines.forEach((line, index) => {
      const lineNum = index + 1;
      const upperLine = line.toUpperCase().trim();
      
      // Extract procedure name
      if (upperLine.includes('PROCEDURE') && upperLine.includes('OPTIONS')) {
        const match = line.match(/^(\w+):\s*PROCEDURE/i);
        if (match) {
          result.metadata.procedureName = match[1];
          result.metadata.mainProgram = upperLine.includes('MAIN');
        }
      }
      
      // Find CALL statements
      if (upperLine.includes('CALL') && !upperLine.startsWith('/*')) {
        const callMatch = line.match(/CALL\s+(\w+)/i);
        if (callMatch) {
          const calledProgram = callMatch[1];
          if (!result.dependencies.programs.includes(calledProgram)) {
            result.dependencies.programs.push(calledProgram);
          }
        }
      }
      
      // Find EXEC SQL
      if (upperLine.includes('EXEC SQL')) {
        result.metadata.hasSQL = true;
      }
    });
    
    return result;
  }
}

// Analysis statistics
class AnalysisStats {
  constructor() {
    this.totalAssets = 0;
    this.byType = {
      COBOL: 0,
      COPYBOOK: 0,
      PLI: 0
    };
    this.dependencies = {
      totalCalls: 0,
      uniquePrograms: new Set(),
      missingPrograms: new Set(),
      circularDependencies: []
    };
    this.features = {
      withSQL: 0,
      withCICS: 0,
      withUnsupported: 0
    };
    this.complexityMetrics = {
      avgDependencies: 0,
      maxDependencies: 0,
      orphanPrograms: 0,
      hubPrograms: [] // Programs called by many others
    };
  }
}

// Main analysis function
function analyzeDemoAssets() {
  const demoDir = '/data/assets/DEMO';
  const cobolAnalyzer = new CobolAnalyzer();
  const copybookAnalyzer = new CopybookAnalyzer();
  const pliAnalyzer = new PLIAnalyzer();
  
  const allResults = [];
  const stats = new AnalysisStats();
  
  console.log('🔍 OpenASP AX Demo Asset Analyzer');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log(`📁 Analyzing demo assets in: ${demoDir}`);
  console.log('');
  
  // Load all files (no extensions)
  const cobolFiles = fs.readdirSync(path.join(demoDir, 'COBOL')).map(f => path.join(demoDir, 'COBOL', f));
  const copybookFiles = fs.readdirSync(path.join(demoDir, 'COPYBOOK')).map(f => path.join(demoDir, 'COPYBOOK', f));
  const pliFiles = fs.readdirSync(path.join(demoDir, 'PLI')).map(f => path.join(demoDir, 'PLI', f));
  
  const totalFiles = cobolFiles.length + copybookFiles.length + pliFiles.length;
  console.log(`📊 Found ${totalFiles} files to analyze`);
  console.log(`   - COBOL: ${cobolFiles.length}`);
  console.log(`   - COPYBOOK: ${copybookFiles.length}`);
  console.log(`   - PL/I: ${pliFiles.length}`);
  console.log('');
  
  let processedCount = 0;
  
  // Analyze COPYBOOKS first
  console.log('📄 Analyzing COPYBOOKS...');
  copybookFiles.forEach(filePath => {
    try {
      const content = fs.readFileSync(filePath, 'utf8');
      const result = copybookAnalyzer.analyze(filePath, content);
      allResults.push(result);
      stats.byType.COPYBOOK++;
      processedCount++;
      
      if (processedCount % 100 === 0) {
        console.log(`   Processed ${processedCount}/${totalFiles} files...`);
      }
    } catch (error) {
      console.error(`   ❌ Error analyzing ${filePath}: ${error.message}`);
    }
  });
  
  // Analyze COBOL programs
  console.log('\n📄 Analyzing COBOL programs...');
  cobolFiles.forEach(filePath => {
    try {
      const content = fs.readFileSync(filePath, 'utf8');
      const result = cobolAnalyzer.analyze(filePath, content);
      allResults.push(result);
      stats.byType.COBOL++;
      
      // Update statistics
      if (result.metadata.hasSQL) stats.features.withSQL++;
      if (result.metadata.hasCICS) stats.features.withCICS++;
      if (result.unsupportedStatements.length > 0) stats.features.withUnsupported++;
      
      processedCount++;
      if (processedCount % 100 === 0) {
        console.log(`   Processed ${processedCount}/${totalFiles} files...`);
      }
    } catch (error) {
      console.error(`   ❌ Error analyzing ${filePath}: ${error.message}`);
    }
  });
  
  // Analyze PL/I programs
  console.log('\n📄 Analyzing PL/I programs...');
  pliFiles.forEach(filePath => {
    try {
      const content = fs.readFileSync(filePath, 'utf8');
      const result = pliAnalyzer.analyze(filePath, content);
      allResults.push(result);
      stats.byType.PLI++;
      
      if (result.metadata.hasSQL) stats.features.withSQL++;
      
      processedCount++;
      if (processedCount % 100 === 0) {
        console.log(`   Processed ${processedCount}/${totalFiles} files...`);
      }
    } catch (error) {
      console.error(`   ❌ Error analyzing ${filePath}: ${error.message}`);
    }
  });
  
  console.log(`\n✅ Analysis complete! Processed ${processedCount} files.`);
  
  // Build dependency graph
  console.log('\n📊 Building dependency graph...');
  const dependencyGraph = {};
  const availablePrograms = new Set();
  const calledPrograms = {};
  
  allResults.forEach(result => {
    if (result.metadata.programId || result.metadata.procedureName) {
      const programName = (result.metadata.programId || result.metadata.procedureName).replace(/\.$/, '');
      availablePrograms.add(programName);
      availablePrograms.add(result.fileName); // No extension to remove
    }
    
    // Track dependencies (no file extensions to remove)
    const programName = result.fileName;
    dependencyGraph[programName] = {
      type: result.fileType,
      calls: result.dependencies.programs,
      copybooks: result.dependencies.copybooks,
      hasSQL: result.metadata.hasSQL,
      hasCICS: result.metadata.hasCICS,
      unsupported: result.unsupportedStatements.length
    };
    
    // Count how many times each program is called
    result.dependencies.programs.forEach(called => {
      calledPrograms[called] = (calledPrograms[called] || 0) + 1;
      stats.dependencies.uniquePrograms.add(called);
    });
    
    stats.dependencies.totalCalls += result.dependencies.programs.length;
  });
  
  // Find missing dependencies
  console.log('\n🔍 Checking for missing dependencies...');
  let missingCount = 0;
  allResults.forEach(result => {
    result.dependencies.programs.forEach(prog => {
      if (!availablePrograms.has(prog) && !prog.startsWith('MISSING')) {
        stats.dependencies.missingPrograms.add(prog);
        missingCount++;
      }
    });
  });
  
  // Find circular dependencies
  console.log('\n🔄 Detecting circular dependencies...');
  function findCircularDeps(node, visited = new Set(), path = []) {
    if (visited.has(node)) {
      const cycleStart = path.indexOf(node);
      if (cycleStart !== -1) {
        const cycle = path.slice(cycleStart).concat(node);
        stats.dependencies.circularDependencies.push(cycle);
        return;
      }
    }
    
    visited.add(node);
    path.push(node);
    
    const deps = dependencyGraph[node];
    if (deps && deps.calls) {
      deps.calls.forEach(dep => {
        if (dependencyGraph[dep]) {
          findCircularDeps(dep, new Set(visited), [...path]);
        }
      });
    }
  }
  
  Object.keys(dependencyGraph).forEach(prog => {
    findCircularDeps(prog);
  });
  
  // Calculate complexity metrics
  console.log('\n📈 Calculating complexity metrics...');
  let totalDeps = 0;
  let maxDeps = 0;
  
  Object.values(dependencyGraph).forEach(info => {
    const depCount = info.calls.length + info.copybooks.length;
    totalDeps += depCount;
    maxDeps = Math.max(maxDeps, depCount);
  });
  
  stats.complexityMetrics.avgDependencies = (totalDeps / Object.keys(dependencyGraph).length).toFixed(2);
  stats.complexityMetrics.maxDependencies = maxDeps;
  
  // Find hub programs (called by many others)
  const hubThreshold = 10;
  Object.entries(calledPrograms)
    .filter(([prog, count]) => count >= hubThreshold)
    .sort((a, b) => b[1] - a[1])
    .slice(0, 10)
    .forEach(([prog, count]) => {
      stats.complexityMetrics.hubPrograms.push({ program: prog, calledBy: count });
    });
  
  // Find orphan programs (not called by anyone)
  const calledSet = new Set(Object.keys(calledPrograms));
  const orphans = Object.keys(dependencyGraph).filter(prog => 
    !calledSet.has(prog) && dependencyGraph[prog].type !== 'COPYBOOK'
  );
  stats.complexityMetrics.orphanPrograms = orphans.length;
  
  // Generate comprehensive report
  console.log('\n📋 ANALYSIS REPORT');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  
  console.log('\n📊 Asset Summary:');
  console.log(`   Total Assets: ${allResults.length}`);
  console.log(`   - COBOL Programs: ${stats.byType.COBOL}`);
  console.log(`   - COPYBOOKS: ${stats.byType.COPYBOOK}`);
  console.log(`   - PL/I Programs: ${stats.byType.PLI}`);
  
  console.log('\n🔗 Dependency Analysis:');
  console.log(`   Total Program Calls: ${stats.dependencies.totalCalls}`);
  console.log(`   Unique Programs Referenced: ${stats.dependencies.uniquePrograms.size}`);
  console.log(`   Missing Programs: ${stats.dependencies.missingPrograms.size}`);
  console.log(`   Circular Dependencies Found: ${stats.dependencies.circularDependencies.length}`);
  
  console.log('\n💡 Feature Usage:');
  console.log(`   Programs with SQL: ${stats.features.withSQL} (${(stats.features.withSQL/stats.byType.COBOL*100).toFixed(1)}%)`);
  console.log(`   Programs with CICS: ${stats.features.withCICS} (${(stats.features.withCICS/stats.byType.COBOL*100).toFixed(1)}%)`);
  console.log(`   Programs with Unsupported Features: ${stats.features.withUnsupported}`);
  
  console.log('\n📈 Complexity Metrics:');
  console.log(`   Average Dependencies per Program: ${stats.complexityMetrics.avgDependencies}`);
  console.log(`   Maximum Dependencies in a Program: ${stats.complexityMetrics.maxDependencies}`);
  console.log(`   Orphan Programs (not called): ${stats.complexityMetrics.orphanPrograms}`);
  
  if (stats.complexityMetrics.hubPrograms.length > 0) {
    console.log('\n🌟 Hub Programs (most frequently called):');
    stats.complexityMetrics.hubPrograms.forEach(hub => {
      console.log(`   - ${hub.program}: called by ${hub.calledBy} programs`);
    });
  }
  
  if (stats.dependencies.circularDependencies.length > 0) {
    console.log('\n🔄 Circular Dependencies (first 5):');
    stats.dependencies.circularDependencies.slice(0, 5).forEach((cycle, i) => {
      console.log(`   ${i + 1}. ${cycle.join(' → ')}`);
    });
  }
  
  // Find programs with unsupported features
  const unsupportedPrograms = allResults.filter(r => r.unsupportedStatements.length > 0);
  if (unsupportedPrograms.length > 0) {
    console.log('\n⚠️  Programs with Unsupported Features:');
    const unsupportedSummary = {};
    unsupportedPrograms.forEach(prog => {
      prog.unsupportedStatements.forEach(stmt => {
        unsupportedSummary[stmt.reason] = (unsupportedSummary[stmt.reason] || 0) + 1;
      });
    });
    
    Object.entries(unsupportedSummary).forEach(([reason, count]) => {
      console.log(`   - ${reason}: ${count} occurrences`);
    });
  }
  
  // Save detailed results
  const reportPath = path.join(demoDir, 'analysis-report.json');
  const report = {
    analysisDate: new Date().toISOString(),
    summary: stats,
    dependencyGraph: dependencyGraph,
    missingPrograms: Array.from(stats.dependencies.missingPrograms),
    circularDependencies: stats.dependencies.circularDependencies.slice(0, 20),
    unsupportedPrograms: unsupportedPrograms.map(p => ({
      program: p.fileName,
      statements: p.unsupportedStatements
    }))
  };
  
  fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));
  console.log(`\n💾 Detailed report saved to: ${reportPath}`);
  
  // Generate migration recommendations
  console.log('\n🎯 MIGRATION RECOMMENDATIONS');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  
  console.log('\n1. Priority Actions:');
  console.log(`   ✓ Resolve ${stats.dependencies.missingPrograms.size} missing program dependencies`);
  console.log(`   ✓ Address ${stats.dependencies.circularDependencies.length} circular dependencies`);
  console.log(`   ✓ Handle ${stats.features.withUnsupported} programs with unsupported features`);
  
  console.log('\n2. Modernization Opportunities:');
  console.log(`   ✓ ${stats.features.withSQL} programs use embedded SQL - consider ORM migration`);
  console.log(`   ✓ ${stats.features.withCICS} programs use CICS - evaluate REST API conversion`);
  console.log(`   ✓ ${stats.complexityMetrics.hubPrograms.length} hub programs - candidates for service layer`);
  
  console.log('\n3. Risk Assessment:');
  const riskLevel = stats.dependencies.circularDependencies.length > 50 ? 'HIGH' : 
                   stats.dependencies.circularDependencies.length > 20 ? 'MEDIUM' : 'LOW';
  console.log(`   ✓ Circular Dependency Risk: ${riskLevel}`);
  console.log(`   ✓ Missing Dependency Risk: ${stats.dependencies.missingPrograms.size > 100 ? 'HIGH' : 'MEDIUM'}`);
  console.log(`   ✓ Technical Debt Score: ${((stats.features.withUnsupported / allResults.length) * 100).toFixed(1)}%`);
  
  return report;
}

// Run the analysis
if (require.main === module) {
  analyzeDemoAssets();
}

module.exports = { analyzeDemoAssets };