#!/usr/bin/env node

/**
 * CL Asset Analysis Script for OpenASP AX
 * Analyzes CL programs and generates comprehensive reports with program relation trees
 */

const fs = require('fs');
const path = require('path');

// CL Analyzer (JavaScript port)
class CLAnalyzer {
  constructor() {
    this.analyzedAssets = new Set();
  }

  analyze(filePath, content) {
    const lines = content.split('\n');
    const fileName = path.basename(filePath);
    
    const result = {
      fileName,
      filePath,
      fileType: 'CL',
      encoding: this.detectEncoding(content),
      size: content.length,
      lineCount: lines.length,
      dependencies: {
        programs: [],
        libraries: [],
        files: []
      },
      missingAssets: [],
      unsupportedStatements: [],
      metadata: {
        programName: '',
        parameters: [],
        variables: [],
        hasErrorHandling: false,
        hasFileOperations: false,
        hasSortOperations: false,
        hasLoops: false,
        controlStructures: [],
        fileOperations: []
      },
      analysisDate: new Date().toISOString()
    };

    this.analyzedAssets.add(fileName);
    
    // Parse CL content line by line
    lines.forEach((line, index) => {
      const lineNum = index + 1;
      const trimmedLine = line.trim();
      const upperLine = trimmedLine.toUpperCase();
      
      // Skip comments and empty lines
      if (trimmedLine.startsWith('*') || trimmedLine === '') {
        return;
      }
      
      // Extract program name
      if (upperLine.startsWith('PGM')) {
        result.metadata.programName = this.extractProgramName(trimmedLine);
      }
      
      // Extract parameters
      if (upperLine.startsWith('PARA')) {
        result.metadata.parameters = this.extractParameters(trimmedLine);
      }
      
      // Extract variables
      if (upperLine.startsWith('VAR')) {
        const variable = this.extractVariable(trimmedLine);
        if (variable) {
          result.metadata.variables.push(variable);
        }
      }
      
      // Extract library definitions
      if (upperLine.includes('DEFLIBL') || upperLine.includes('LIBL-')) {
        const library = this.extractLibrary(trimmedLine);
        if (library && !result.dependencies.libraries.includes(library)) {
          result.dependencies.libraries.push(library);
        }
      }
      
      // Extract CALL statements
      if (upperLine.includes('CALL') && upperLine.includes('PGM-')) {
        const programCall = this.extractProgramCall(trimmedLine);
        if (programCall) {
          if (!result.dependencies.programs.includes(programCall.program)) {
            result.dependencies.programs.push(programCall.program);
          }
          if (programCall.library && !result.dependencies.libraries.includes(programCall.library)) {
            result.dependencies.libraries.push(programCall.library);
          }
        }
      }
      
      // Extract file operations
      if (this.isFileOperation(upperLine)) {
        result.metadata.hasFileOperations = true;
        const fileOp = this.extractFileOperation(trimmedLine, lineNum);
        if (fileOp) {
          result.metadata.fileOperations.push(fileOp);
          if (!result.dependencies.files.includes(fileOp.fileName)) {
            result.dependencies.files.push(fileOp.fileName);
          }
        }
      }
      
      // Extract SORTD operations
      if (upperLine.includes('SORTD')) {
        result.metadata.hasSortOperations = true;
        const sortFiles = this.extractSortFiles(trimmedLine);
        sortFiles.forEach(file => {
          if (!result.dependencies.files.includes(file)) {
            result.dependencies.files.push(file);
          }
        });
      }
      
      // Extract control structures
      const controlStructure = this.extractControlStructure(upperLine, lineNum);
      if (controlStructure) {
        result.metadata.controlStructures.push(controlStructure);
        if (controlStructure.type === 'FOR' || controlStructure.type === 'WHILE') {
          result.metadata.hasLoops = true;
        }
      }
      
      // Check for error handling
      if (upperLine.includes('ERROR') || upperLine.includes('GOTO') || 
          (upperLine.includes('IF') && upperLine.includes('RETCODE'))) {
        result.metadata.hasErrorHandling = true;
      }
      
      // Check for unsupported statements
      const unsupported = this.checkUnsupportedStatements(trimmedLine, lineNum);
      if (unsupported) {
        result.unsupportedStatements.push(unsupported);
      }
    });
    
    return result;
  }

  detectEncoding(content) {
    // Simple heuristic for EBCDIC detection
    const ebcdicPattern = /[\x40-\x7F\x81-\x9F\xA2-\xFE]/;
    return ebcdicPattern.test(content) ? 'EBCDIC' : 'ASCII';
  }

  extractProgramName(line) {
    const match = line.match(/PGM\s*\(\s*([^)]+)\s*\)/i);
    return match ? match[1].trim() : '';
  }

  extractParameters(line) {
    const match = line.match(/PARA\s+(.+)/i);
    if (match) {
      return match[1].split(',').map(param => param.trim());
    }
    return [];
  }

  extractVariable(line) {
    const match = line.match(/VAR\s+([^,]+),(.+)/i);
    if (match) {
      return {
        name: match[1].trim(),
        type: match[2].trim()
      };
    }
    return null;
  }

  extractLibrary(line) {
    const match = line.match(/LIBL-([A-Z0-9]+)/i);
    return match ? match[1] : null;
  }

  extractProgramCall(line) {
    const match = line.match(/CALL\s+PGM-([^.]+)(?:\.([A-Z0-9]+))?/i);
    if (match) {
      return {
        program: match[1],
        library: match[2] || undefined
      };
    }
    return null;
  }

  isFileOperation(line) {
    const fileOps = ['CRTFILE', 'DLTFILE', 'CPYFILE', 'MOVFILE', 'BAKFILE'];
    return fileOps.some(op => line.includes(op));
  }

  extractFileOperation(line, lineNum) {
    const fileOps = ['CRTFILE', 'DLTFILE', 'CPYFILE', 'MOVFILE', 'BAKFILE'];
    
    for (const op of fileOps) {
      if (line.toUpperCase().includes(op)) {
        const match = line.match(new RegExp(`${op}\\s+FILE-([A-Z0-9]+)`, 'i'));
        if (match) {
          return {
            type: op,
            fileName: match[1],
            line: lineNum
          };
        }
      }
    }
    return null;
  }

  extractSortFiles(line) {
    const files = [];
    const infileMatch = line.match(/INFILE-([A-Z0-9]+)/i);
    const outfileMatch = line.match(/OUTFILE-([A-Z0-9]+)/i);
    
    if (infileMatch) files.push(infileMatch[1]);
    if (outfileMatch) files.push(outfileMatch[1]);
    
    return files;
  }

  extractControlStructure(line, lineNum) {
    if (line.startsWith('IF ')) return { type: 'IF', line: lineNum };
    if (line.startsWith('CASE ')) return { type: 'CASE', line: lineNum };
    if (line.startsWith('FOR ')) return { type: 'FOR', line: lineNum };
    if (line.startsWith('WHILE ')) return { type: 'WHILE', line: lineNum };
    return null;
  }

  checkUnsupportedStatements(line, lineNum) {
    const upperLine = line.toUpperCase();
    
    if (upperLine.includes('EXEC SQL')) {
      return {
        line: lineNum,
        statement: line.trim(),
        reason: 'Embedded SQL in CL is not commonly supported'
      };
    }
    
    if (upperLine.includes('EXEC CICS')) {
      return {
        line: lineNum,
        statement: line.trim(),
        reason: 'CICS commands in CL may require special handling'
      };
    }
    
    if (upperLine.includes('DYNVAR')) {
      return {
        line: lineNum,
        statement: line.trim(),
        reason: 'Dynamic variables may not be supported in all environments'
      };
    }
    
    return null;
  }
}

// Program relation tree builder
class ProgramRelationTree {
  constructor() {
    this.nodes = new Map();
    this.edges = [];
  }

  addProgram(program, type = 'UNKNOWN') {
    if (!this.nodes.has(program)) {
      this.nodes.set(program, {
        name: program,
        type: type,
        children: [],
        parents: [],
        level: 0,
        isRoot: true
      });
    }
  }

  addRelation(from, to, relationType = 'CALLS') {
    this.addProgram(from, 'CL');
    this.addProgram(to, 'COBOL');
    
    const fromNode = this.nodes.get(from);
    const toNode = this.nodes.get(to);
    
    if (!fromNode.children.includes(to)) {
      fromNode.children.push(to);
    }
    
    if (!toNode.parents.includes(from)) {
      toNode.parents.push(from);
      toNode.isRoot = false;
    }
    
    this.edges.push({
      from: from,
      to: to,
      type: relationType
    });
  }

  calculateLevels() {
    // Calculate levels for tree visualization
    const visited = new Set();
    
    const calculateLevel = (nodeName, level = 0) => {
      if (visited.has(nodeName)) return;
      visited.add(nodeName);
      
      const node = this.nodes.get(nodeName);
      if (node) {
        node.level = Math.max(node.level, level);
        node.children.forEach(child => {
          calculateLevel(child, level + 1);
        });
      }
    };
    
    // Start from root nodes (CL programs with no parents)
    for (const [name, node] of this.nodes) {
      if (node.isRoot && node.type === 'CL') {
        calculateLevel(name, 0);
      }
    }
  }

  getTreeStructure() {
    this.calculateLevels();
    
    const roots = [];
    for (const [name, node] of this.nodes) {
      if (node.isRoot && node.type === 'CL') {
        roots.push(this.buildSubTree(name));
      }
    }
    
    return roots;
  }

  buildSubTree(nodeName) {
    const node = this.nodes.get(nodeName);
    if (!node) return null;
    
    return {
      name: nodeName,
      type: node.type,
      level: node.level,
      children: node.children.map(child => this.buildSubTree(child)).filter(Boolean)
    };
  }
}

// Main analysis function
function analyzeCLAssets() {
  const clDir = '/data/assets/SRC.CLLIB';
  const clAnalyzer = new CLAnalyzer();
  const relationTree = new ProgramRelationTree();
  
  console.log('🔍 OpenASP AX CL Asset Analyzer');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log(`📁 Analyzing CL assets in: ${clDir}`);
  console.log('');
  
  const allResults = [];
  const stats = {
    totalCLPrograms: 0,
    totalProgramCalls: 0,
    totalLibraries: new Set(),
    totalFiles: new Set(),
    programsWithErrorHandling: 0,
    programsWithFileOps: 0,
    programsWithSortOps: 0,
    programsWithLoops: 0,
    unsupportedFeatures: 0,
    calledPrograms: new Map()
  };
  
  try {
    // Load all CL files
    const clFiles = fs.readdirSync(clDir)
      .filter(f => !f.endsWith('.json'))
      .map(f => path.join(clDir, f));
    
    console.log(`📊 Found ${clFiles.length} CL files to analyze`);
    console.log('');
    
    // Analyze each CL file
    console.log('📄 Analyzing CL programs...');
    clFiles.forEach((filePath, index) => {
      try {
        const content = fs.readFileSync(filePath, 'utf8');
        const result = clAnalyzer.analyze(filePath, content);
        allResults.push(result);
        
        // Update statistics
        stats.totalCLPrograms++;
        stats.totalProgramCalls += result.dependencies.programs.length;
        
        result.dependencies.libraries.forEach(lib => stats.totalLibraries.add(lib));
        result.dependencies.files.forEach(file => stats.totalFiles.add(file));
        
        if (result.metadata.hasErrorHandling) stats.programsWithErrorHandling++;
        if (result.metadata.hasFileOperations) stats.programsWithFileOps++;
        if (result.metadata.hasSortOperations) stats.programsWithSortOps++;
        if (result.metadata.hasLoops) stats.programsWithLoops++;
        if (result.unsupportedStatements.length > 0) stats.unsupportedFeatures++;
        
        // Build relation tree
        const clName = result.metadata.programName || result.fileName;
        result.dependencies.programs.forEach(program => {
          relationTree.addRelation(clName, program, 'CALLS');
          
          // Count how many times each program is called
          const count = stats.calledPrograms.get(program) || 0;
          stats.calledPrograms.set(program, count + 1);
        });
        
        if ((index + 1) % 20 === 0) {
          console.log(`   Processed ${index + 1}/${clFiles.length} files...`);
        }
      } catch (error) {
        console.error(`   ❌ Error analyzing ${filePath}: ${error.message}`);
      }
    });
    
    console.log(`\n✅ Analysis complete! Processed ${stats.totalCLPrograms} CL programs.`);
    
    // Generate program relation tree
    console.log('\n🌳 Building program relation tree...');
    const treeStructure = relationTree.getTreeStructure();
    
    // Generate comprehensive report
    console.log('\n📋 CL ANALYSIS REPORT');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    console.log('\n📊 CL Program Summary:');
    console.log(`   Total CL Programs: ${stats.totalCLPrograms}`);
    console.log(`   Total Program Calls: ${stats.totalProgramCalls}`);
    console.log(`   Unique Libraries Referenced: ${stats.totalLibraries.size}`);
    console.log(`   Files Referenced: ${stats.totalFiles.size}`);
    
    console.log('\n🔧 Feature Usage:');
    console.log(`   Programs with Error Handling: ${stats.programsWithErrorHandling} (${(stats.programsWithErrorHandling/stats.totalCLPrograms*100).toFixed(1)}%)`);
    console.log(`   Programs with File Operations: ${stats.programsWithFileOps} (${(stats.programsWithFileOps/stats.totalCLPrograms*100).toFixed(1)}%)`);
    console.log(`   Programs with Sort Operations: ${stats.programsWithSortOps} (${(stats.programsWithSortOps/stats.totalCLPrograms*100).toFixed(1)}%)`);
    console.log(`   Programs with Loops: ${stats.programsWithLoops} (${(stats.programsWithLoops/stats.totalCLPrograms*100).toFixed(1)}%)`);
    console.log(`   Programs with Unsupported Features: ${stats.unsupportedFeatures}`);
    
    // Show most called programs
    const sortedCalls = Array.from(stats.calledPrograms.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 10);
    
    if (sortedCalls.length > 0) {
      console.log('\n🌟 Most Called Programs:');
      sortedCalls.forEach(([program, count]) => {
        console.log(`   - ${program}: called by ${count} CL programs`);
      });
    }
    
    // Show program relation tree (simplified view)
    console.log('\n🌳 Program Relation Tree (Top 5 CL Programs):');
    treeStructure.slice(0, 5).forEach(root => {
      console.log(`📋 ${root.name} (${root.type})`);
      if (root.children && root.children.length > 0) {
        root.children.forEach(child => {
          console.log(`   └── ${child.name} (${child.type})`);
          if (child.children && child.children.length > 0) {
            child.children.slice(0, 3).forEach(grandchild => {
              console.log(`       └── ${grandchild.name} (${grandchild.type})`);
            });
            if (child.children.length > 3) {
              console.log(`       └── ... and ${child.children.length - 3} more`);
            }
          }
        });
      }
    });
    
    // Save detailed results
    const reportPath = path.join(clDir, 'cl-analysis-report.json');
    const report = {
      analysisDate: new Date().toISOString(),
      summary: {
        totalCLPrograms: stats.totalCLPrograms,
        totalProgramCalls: stats.totalProgramCalls,
        uniqueLibraries: Array.from(stats.totalLibraries),
        referencedFiles: Array.from(stats.totalFiles),
        featureUsage: {
          errorHandling: stats.programsWithErrorHandling,
          fileOperations: stats.programsWithFileOps,
          sortOperations: stats.programsWithSortOps,
          loops: stats.programsWithLoops,
          unsupportedFeatures: stats.unsupportedFeatures
        }
      },
      programRelationTree: treeStructure,
      calledPrograms: Object.fromEntries(stats.calledPrograms),
      detailedResults: allResults
    };
    
    fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));
    console.log(`\n💾 Detailed report saved to: ${reportPath}`);
    
    console.log('\n🎯 MIGRATION RECOMMENDATIONS');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    console.log('\n1. Priority Actions:');
    console.log(`   ✓ Review ${stats.unsupportedFeatures} programs with unsupported features`);
    console.log(`   ✓ Modernize ${stats.programsWithFileOps} programs using file operations`);
    console.log(`   ✓ Consider automation for ${stats.programsWithSortOps} programs with sort operations`);
    
    console.log('\n2. Architecture Insights:');
    console.log(`   ✓ ${treeStructure.length} entry-point CL programs identified`);
    console.log(`   ✓ Average ${(stats.totalProgramCalls/stats.totalCLPrograms).toFixed(1)} program calls per CL`);
    console.log(`   ✓ ${stats.totalLibraries.size} libraries in use - consider consolidation`);
    
    return report;
    
  } catch (error) {
    console.error(`❌ Error during analysis: ${error.message}`);
    return null;
  }
}

// Run the analysis
if (require.main === module) {
  analyzeCLAssets();
}

module.exports = { analyzeCLAssets, CLAnalyzer, ProgramRelationTree };