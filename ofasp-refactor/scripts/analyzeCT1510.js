#!/usr/bin/env node

/**
 * CT1510 File Analysis Script
 * Path: /home/aspuser/app/ofasp-refactor/scripts/analyzeCT1510.js
 * 
 * This script analyzes the CT1510 file and determines its type
 */

const fs = require('fs');
const path = require('path');

// File type definitions
const FILE_TYPES = {
  COBOL: 'COBOL',
  COPYBOOK: 'COPYBOOK', 
  CL: 'CL',
  SMED: 'SMED',
  UNKNOWN: 'UNKNOWN'
};

class FileAnalyzer {
  analyzeFile(fileName, content, fileSize) {
    const lines = content.split('\n');
    const upperContent = content.toUpperCase();
    
    const result = {
      fileName,
      fileType: FILE_TYPES.UNKNOWN,
      confidence: 0,
      detectionReasons: [],
      encoding: this.detectEncoding(content),
      size: fileSize,
      lineCount: lines.length,
      hasSQL: this.hasSQL(upperContent),
      hasMapStatements: this.hasMapStatements(upperContent),
      hasDataDivision: this.hasDataDivision(upperContent),
      analysisTimestamp: new Date().toISOString()
    };

    // Analyze file type with scoring system
    const scores = {
      [FILE_TYPES.COBOL]: this.analyzeCobol(fileName, upperContent, lines),
      [FILE_TYPES.COPYBOOK]: this.analyzeCopybook(fileName, upperContent, lines),
      [FILE_TYPES.CL]: this.analyzeCL(fileName, upperContent, lines),
      [FILE_TYPES.SMED]: this.analyzeSMED(fileName, upperContent, lines)
    };

    // Find the highest score
    let maxScore = 0;
    let detectedType = FILE_TYPES.UNKNOWN;
    
    for (const [type, analysis] of Object.entries(scores)) {
      if (analysis.score > maxScore) {
        maxScore = analysis.score;
        detectedType = type;
        result.detectionReasons = analysis.reasons;
      }
    }

    result.fileType = detectedType;
    result.confidence = Math.min(maxScore, 100);

    return result;
  }

  analyzeCobol(fileName, content, lines) {
    let score = 0;
    const reasons = [];

    // File extension check
    const ext = fileName.toLowerCase().split('.').pop() || '';
    if (['cob', 'cobol', 'cbl'].includes(ext)) {
      score += 20;
      reasons.push(`COBOL file extension (.${ext})`);
    }

    // COBOL Division keywords
    const divisions = [
      'IDENTIFICATION DIVISION',
      'ENVIRONMENT DIVISION', 
      'DATA DIVISION',
      'PROCEDURE DIVISION'
    ];
    
    divisions.forEach(division => {
      if (content.includes(division)) {
        score += 15;
        reasons.push(`Contains ${division}`);
      }
    });

    // COBOL specific keywords
    const cobolKeywords = [
      'PROGRAM-ID',
      'WORKING-STORAGE SECTION',
      'FILE SECTION',
      'LINKAGE SECTION',
      'EXEC SQL',
      'EXEC CICS'
    ];

    cobolKeywords.forEach(keyword => {
      if (content.includes(keyword)) {
        score += 8;
        reasons.push(`Contains ${keyword}`);
      }
    });

    // COBOL PIC clauses
    if (content.match(/PIC\s+[X9]+/)) {
      score += 10;
      reasons.push('Contains PIC clauses');
    }

    // COBOL COPY statements
    if (content.match(/COPY\s+\w+/)) {
      score += 8;
      reasons.push('Contains COPY statements');
    }

    // Check for typical COBOL structure (01 level items)
    const hasLevelNumbers = lines.some(line => 
      line.trim().match(/^\d{2}\s+[\w-]+/)
    );
    if (hasLevelNumbers) {
      score += 12;
      reasons.push('Contains COBOL level numbers');
    }

    return { score, reasons };
  }

  analyzeCopybook(fileName, content, lines) {
    let score = 0;
    const reasons = [];

    // File extension check
    const ext = fileName.toLowerCase().split('.').pop() || '';
    if (['cpy', 'copy', 'inc', 'copybook'].includes(ext)) {
      score += 25;
      reasons.push(`COPYBOOK file extension (.${ext})`);
    }

    // Copybooks typically don't have divisions
    const hasDivisions = [
      'IDENTIFICATION DIVISION',
      'PROCEDURE DIVISION'
    ].some(div => content.includes(div));
    
    if (!hasDivisions) {
      score += 15;
      reasons.push('No program divisions (typical of copybooks)');
    } else {
      score -= 10; // Penalize if it has divisions
    }

    // Data structure definitions
    const hasDataStructures = lines.some(line => 
      line.trim().match(/^\d{2}\s+[\w-]+.*PIC/)
    );
    if (hasDataStructures) {
      score += 15;
      reasons.push('Contains data structure definitions');
    }

    return { score, reasons };
  }

  analyzeCL(fileName, content, lines) {
    let score = 0;
    const reasons = [];

    // File extension check
    const ext = fileName.toLowerCase().split('.').pop() || '';
    if (['cl', 'cle', 'clp', 'cmd'].includes(ext)) {
      score += 20;
      reasons.push(`CL file extension (.${ext})`);
    }

    // CL specific commands
    const clCommands = [
      'PGM',
      'ENDPGM',
      'DCL VAR',
      'CHGVAR',
      'CALL',
      'SBMJOB',
      'CPYF',
      'DLTF',
      'CRTPF'
    ];

    clCommands.forEach(cmd => {
      if (content.includes(cmd)) {
        score += 12;
        reasons.push(`Contains CL command: ${cmd}`);
      }
    });

    return { score, reasons };
  }

  analyzeSMED(fileName, content, lines) {
    let score = 0;
    const reasons = [];

    // File extension check
    const ext = fileName.toLowerCase().split('.').pop() || '';
    if (['smed', 'map', 'bms'].includes(ext)) {
      score += 25;
      reasons.push(`SMED/Map file extension (.${ext})`);
    }

    // SMED specific keywords
    const smedKeywords = [
      'SMED',
      'DFHMSD',
      'DFHMDI',
      'DFHMDF'
    ];

    smedKeywords.forEach(keyword => {
      if (content.includes(keyword)) {
        score += 10;
        reasons.push(`Contains SMED keyword: ${keyword}`);
      }
    });

    return { score, reasons };
  }

  detectEncoding(content) {
    const hasEBCDICChars = /[\u0081-\u0089\u0091-\u0099\u00A2-\u00A9\u00C1-\u00C9\u00D1-\u00D9\u00E2-\u00E9\u00F0-\u00F9]/.test(content);
    return hasEBCDICChars ? 'EBCDIC' : 'ASCII';
  }

  hasSQL(content) {
    return content.includes('EXEC SQL') || content.includes('EXEC-SQL');
  }

  hasMapStatements(content) {
    return content.includes('SEND MAP') || content.includes('RECEIVE MAP') || content.includes('DFHMSD');
  }

  hasDataDivision(content) {
    return content.includes('DATA DIVISION');
  }
}

// Main analysis function
function analyzeCT1510() {
  const filePath = '/data/CT1510';
  
  try {
    console.log('🔍 Starting CT1510 File Analysis...');
    console.log(`📁 File Path: ${filePath}`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    // Check if file exists
    if (!fs.existsSync(filePath)) {
      console.error(`❌ Error: File ${filePath} does not exist`);
      return null;
    }

    // Read file content
    const content = fs.readFileSync(filePath, 'utf8');
    const stats = fs.statSync(filePath);
    
    console.log(`📊 File Size: ${stats.size} bytes`);
    console.log(`📝 Lines: ${content.split('\n').length}`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    // Analyze the file
    const analyzer = new FileAnalyzer();
    const result = analyzer.analyzeFile('CT1510', content, stats.size);
    
    // Display results
    console.log('📋 ANALYSIS RESULTS:');
    console.log(`   📂 File Name: ${result.fileName}`);
    console.log(`   🏷️  File Type: ${result.fileType}`);
    console.log(`   📊 Confidence: ${result.confidence}%`);
    console.log(`   🔤 Encoding: ${result.encoding}`);
    console.log(`   📏 Size: ${result.size} bytes`);
    console.log(`   📝 Lines: ${result.lineCount}`);
    console.log(`   🗃️  Has SQL: ${result.hasSQL ? '✅' : '❌'}`);
    console.log(`   🗺️  Has Maps: ${result.hasMapStatements ? '✅' : '❌'}`);
    console.log(`   📊 Has Data Division: ${result.hasDataDivision ? '✅' : '❌'}`);
    
    console.log('\n🔍 DETECTION REASONS:');
    result.detectionReasons.forEach((reason, index) => {
      console.log(`   ${index + 1}. ${reason}`);
    });
    
    console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    // Save analysis result to JSON file
    const outputPath = '/home/aspuser/app/ofasp-refactor/analysis-result.json';
    fs.writeFileSync(outputPath, JSON.stringify(result, null, 2));
    console.log(`💾 Analysis result saved to: ${outputPath}`);
    
    return result;
    
  } catch (error) {
    console.error('❌ Error during analysis:', error.message);
    return null;
  }
}

// Run the analysis
if (require.main === module) {
  analyzeCT1510();
}

module.exports = { FileAnalyzer, analyzeCT1510 };