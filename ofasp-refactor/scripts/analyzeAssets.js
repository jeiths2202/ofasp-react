#!/usr/bin/env node

/**
 * Asset Analysis Script for OpenASP AX
 * Analyzes COBOL and COPYBOOK files in /data/assets
 */

const fs = require('fs');
const path = require('path');

// Base Analyzer
class BaseAnalyzer {
  constructor() {
    this.analyzedAssets = new Set();
  }
  
  detectEncoding(content) {
    const hasEBCDICChars = /[\u0081-\u0089\u0091-\u0099\u00A2-\u00A9\u00C1-\u00C9\u00D1-\u00D9\u00E2-\u00E9\u00F0-\u00F9]/.test(content);
    return hasEBCDICChars ? 'EBCDIC' : 'ASCII';
  }
  
  isAnalyzed(assetName) {
    return this.analyzedAssets.has(assetName);
  }
  
  markAsAnalyzed(assetName) {
    this.analyzedAssets.add(assetName);
  }
}

// COBOL Analyzer
class CobolAnalyzer extends BaseAnalyzer {
  analyze(filePath, content) {
    const lines = content.split('\n');
    const fileName = path.basename(filePath);
    
    const result = {
      fileName,
      filePath,
      fileType: 'COBOL',
      encoding: this.detectEncoding(content),
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
        programId: '',
        hasSQL: false,
        hasCICS: false,
        divisions: []
      },
      analysisDate: new Date().toISOString()
    };

    this.markAsAnalyzed(fileName);

    lines.forEach((line, index) => {
      const lineNum = index + 1;
      const upperLine = line.toUpperCase().trim();
      
      // Extract PROGRAM-ID
      if (upperLine.includes('PROGRAM-ID')) {
        const match = line.match(/PROGRAM-ID\.\s*(\S+)/i);
        if (match) {
          result.metadata.programId = match[1];
        }
      }
      
      // Detect divisions
      const divisions = ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE'];
      divisions.forEach(div => {
        if (upperLine.includes(`${div} DIVISION`)) {
          result.metadata.divisions.push(div);
        }
      });
      
      // Find CALL statements
      if (upperLine.includes('CALL')) {
        const callMatch = line.match(/CALL\s+['"]?(\w+)['"]?/i);
        if (callMatch) {
          const calledProgram = callMatch[1];
          if (!result.dependencies.programs.includes(calledProgram)) {
            result.dependencies.programs.push(calledProgram);
          }
        }
      }
      
      // Find COPY statements
      if (upperLine.includes('COPY')) {
        const copyMatch = line.match(/COPY\s+(\w+)/i);
        if (copyMatch) {
          const copybook = copyMatch[1];
          if (!result.dependencies.copybooks.includes(copybook)) {
            result.dependencies.copybooks.push(copybook);
          }
        }
      }
      
      // Find EXEC SQL
      if (upperLine.includes('EXEC SQL') || upperLine.includes('EXEC-SQL')) {
        result.metadata.hasSQL = true;
      }
      
      // Find EXEC CICS
      if (upperLine.includes('EXEC CICS')) {
        result.metadata.hasCICS = true;
      }
      
      // Detect unsupported statements
      const unsupportedPatterns = [
        { pattern: /SORT\s+\w+/, reason: 'SORT statement needs special handling' },
        { pattern: /MERGE\s+\w+/, reason: 'MERGE statement needs special handling' },
        { pattern: /EXEC\s+DLI/, reason: 'DL/I database calls not supported' },
        { pattern: /EXEC\s+IDMS/, reason: 'IDMS database calls not supported' }
      ];
      
      unsupportedPatterns.forEach(({ pattern, reason }) => {
        if (pattern.test(line)) {
          result.unsupportedStatements.push({
            statement: line.trim(),
            lineNumber: lineNum,
            reason
          });
        }
      });
    });
    
    return result;
  }
}

// COPYBOOK Analyzer
class CopybookAnalyzer extends BaseAnalyzer {
  analyze(filePath, content) {
    const lines = content.split('\n');
    const fileName = path.basename(filePath);
    
    const result = {
      fileName,
      filePath,
      fileType: 'COPYBOOK',
      encoding: this.detectEncoding(content),
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
        dataStructures: [],
        totalFields: 0,
        hasRedefines: false,
        hasOccurs: false,
        maxLevel: 0
      },
      analysisDate: new Date().toISOString()
    };

    this.markAsAnalyzed(fileName);

    let currentStructure = null;
    let maxLevel = 0;

    lines.forEach((line, index) => {
      const lineNum = index + 1;
      const trimmedLine = line.trim();
      const upperLine = trimmedLine.toUpperCase();
      
      // Skip comments and empty lines
      if (trimmedLine.startsWith('*') || trimmedLine === '') {
        return;
      }
      
      // Parse level number and field definition
      const levelMatch = trimmedLine.match(/^(\d{2})\s+([\w-]+)(.*)$/);
      if (levelMatch) {
        const level = parseInt(levelMatch[1]);
        const fieldName = levelMatch[2];
        const remainder = levelMatch[3] || '';
        
        maxLevel = Math.max(maxLevel, level);
        result.metadata.totalFields++;
        
        // Track 01 level structures
        if (level === 1) {
          currentStructure = {
            name: fieldName,
            fields: [],
            line: lineNum
          };
          result.metadata.dataStructures.push(currentStructure);
        }
        
        // Check for REDEFINES
        if (upperLine.includes('REDEFINES')) {
          result.metadata.hasRedefines = true;
        }
        
        // Check for OCCURS
        if (upperLine.includes('OCCURS')) {
          result.metadata.hasOccurs = true;
        }
      }
      
      // Check for nested COPY
      if (upperLine.includes('COPY')) {
        const copyMatch = line.match(/COPY\s+(\w+)/i);
        if (copyMatch) {
          const nestedCopybook = copyMatch[1];
          if (!result.dependencies.copybooks.includes(nestedCopybook)) {
            result.dependencies.copybooks.push(nestedCopybook);
          }
        }
      }
    });
    
    result.metadata.maxLevel = maxLevel;
    
    return result;
  }
}

// Main analysis function
function analyzeAssets() {
  const assetsDir = '/data/assets';
  const cobolAnalyzer = new CobolAnalyzer();
  const copybookAnalyzer = new CopybookAnalyzer();
  const allResults = [];
  
  console.log('🔍 OpenASP AX Asset Analyzer');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log(`📁 Analyzing assets in: ${assetsDir}`);
  console.log('');
  
  // Find all files recursively
  function findFiles(dir) {
    const files = [];
    const items = fs.readdirSync(dir);
    
    items.forEach(item => {
      const fullPath = path.join(dir, item);
      const stat = fs.statSync(fullPath);
      
      if (stat.isDirectory()) {
        files.push(...findFiles(fullPath));
      } else {
        files.push(fullPath);
      }
    });
    
    return files;
  }
  
  const allFiles = findFiles(assetsDir);
  console.log(`📊 Found ${allFiles.length} files to analyze\n`);
  
  // Analyze each file
  allFiles.forEach(filePath => {
    try {
      const content = fs.readFileSync(filePath, 'utf8');
      const fileName = path.basename(filePath);
      const fileExt = path.extname(fileName).toLowerCase();
      const dirName = path.basename(path.dirname(filePath));
      
      console.log(`\n📄 Analyzing: ${filePath}`);
      console.log(`   Directory: ${dirName}`);
      
      let result = null;
      
      // Determine file type and use appropriate analyzer
      if (dirName.includes('COBLIB') || fileExt === '.cob' || fileExt === '.cbl') {
        // COBOL file
        console.log('   Type: COBOL (detected by directory/extension)');
        result = cobolAnalyzer.analyze(filePath, content);
      } else if (dirName.includes('CPYLIB') || fileExt === '.cpy' || fileExt === '.copy') {
        // COPYBOOK file
        console.log('   Type: COPYBOOK (detected by directory/extension)');
        result = copybookAnalyzer.analyze(filePath, content);
      } else {
        // Try to detect by content
        const upperContent = content.toUpperCase();
        if (upperContent.includes('IDENTIFICATION DIVISION') || upperContent.includes('PROGRAM-ID')) {
          console.log('   Type: COBOL (detected by content)');
          result = cobolAnalyzer.analyze(filePath, content);
        } else if (upperContent.match(/^\s*\d{2}\s+[\w-]+.*PIC/m)) {
          console.log('   Type: COPYBOOK (detected by content)');
          result = copybookAnalyzer.analyze(filePath, content);
        } else {
          console.log('   Type: UNKNOWN - Skipping');
        }
      }
      
      if (result) {
        allResults.push(result);
        
        // Display analysis results
        console.log(`   ✅ Analysis Complete:`);
        console.log(`      - Encoding: ${result.encoding}`);
        console.log(`      - Size: ${result.size} bytes`);
        console.log(`      - Lines: ${result.lineCount}`);
        
        if (result.fileType === 'COBOL') {
          console.log(`      - Program ID: ${result.metadata.programId || 'Not found'}`);
          console.log(`      - Divisions: ${result.metadata.divisions.join(', ') || 'None'}`);
          console.log(`      - Has SQL: ${result.metadata.hasSQL ? '✅' : '❌'}`);
          console.log(`      - Has CICS: ${result.metadata.hasCICS ? '✅' : '❌'}`);
          console.log(`      - Dependencies:`);
          if (result.dependencies.programs.length > 0) {
            console.log(`        • Programs: ${result.dependencies.programs.join(', ')}`);
          }
          if (result.dependencies.copybooks.length > 0) {
            console.log(`        • Copybooks: ${result.dependencies.copybooks.join(', ')}`);
          }
        } else if (result.fileType === 'COPYBOOK') {
          console.log(`      - Data Structures: ${result.metadata.dataStructures.length}`);
          console.log(`      - Total Fields: ${result.metadata.totalFields}`);
          console.log(`      - Max Level: ${result.metadata.maxLevel}`);
          console.log(`      - Has REDEFINES: ${result.metadata.hasRedefines ? '✅' : '❌'}`);
          console.log(`      - Has OCCURS: ${result.metadata.hasOccurs ? '✅' : '❌'}`);
        }
        
        if (result.unsupportedStatements.length > 0) {
          console.log(`      - ⚠️  Unsupported Statements: ${result.unsupportedStatements.length}`);
          result.unsupportedStatements.forEach(stmt => {
            console.log(`        Line ${stmt.lineNumber}: ${stmt.reason}`);
          });
        }
      }
      
    } catch (error) {
      console.error(`   ❌ Error: ${error.message}`);
    }
  });
  
  // Check for missing dependencies
  console.log('\n\n📋 DEPENDENCY ANALYSIS');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  
  // Create set of available assets - remove trailing periods from program IDs
  const availableAssets = new Set();
  allResults.forEach(r => {
    if (r.metadata.programId) {
      // Add both with and without period
      availableAssets.add(r.metadata.programId.replace(/\.$/, ''));
      availableAssets.add(r.metadata.programId);
    }
    availableAssets.add(r.fileName);
  });
  
  allResults.forEach(result => {
    const missingPrograms = result.dependencies.programs.filter(p => !availableAssets.has(p));
    const missingCopybooks = result.dependencies.copybooks.filter(c => 
      !allResults.some(r => r.fileName === c || r.fileName === `${c}.cpy` || r.fileName === c.toUpperCase())
    );
    
    if (missingPrograms.length > 0 || missingCopybooks.length > 0) {
      console.log(`\n${result.fileName}:`);
      if (missingPrograms.length > 0) {
        console.log(`   ❌ Missing Programs: ${missingPrograms.join(', ')}`);
      }
      if (missingCopybooks.length > 0) {
        console.log(`   ❌ Missing Copybooks: ${missingCopybooks.join(', ')}`);
      }
    }
  });
  
  // Save detailed results
  const outputPath = '/home/aspuser/app/ofasp-refactor/asset-analysis-results.json';
  fs.writeFileSync(outputPath, JSON.stringify(allResults, null, 2));
  console.log(`\n\n💾 Detailed results saved to: ${outputPath}`);
  
  return allResults;
}

// Run the analysis
if (require.main === module) {
  analyzeAssets();
}

module.exports = { CobolAnalyzer, CopybookAnalyzer, analyzeAssets };