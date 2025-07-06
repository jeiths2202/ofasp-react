#!/usr/bin/env node

import * as fs from 'fs';
import * as path from 'path';
import { PureCobolConverter } from '../utils/pure-cobol-converter';

class CobolConverterCLI {
    private converter: PureCobolConverter;
    
    constructor() {
        this.converter = new PureCobolConverter();
    }
    
    private showHelp(): void {
        console.log(`
COBOL to Java Converter CLI
===========================

Usage:
  node cobol-converter.js <input.cob> [output.java]
  node cobol-converter.js --help
  
Arguments:
  input.cob     Path to COBOL source file (.cob, .COB, .cobol)
  output.java   Optional output path for Java file (default: auto-generated)
  
Options:
  --help        Show this help message
  --verbose     Show detailed conversion information
  --check-only  Only check if conversion is possible (no output)
  
Examples:
  node cobol-converter.js TAX01.cob
  node cobol-converter.js TAX01.cob MyTaxProgram.java
  node cobol-converter.js --check-only PAYROLL.cob
  node cobol-converter.js --verbose HELLO.cob
  
Supported COBOL Features:
  ✓ PROGRAM-ID, WORKING-STORAGE SECTION
  ✓ FILE-CONTROL, FILE SECTION, FD records
  ✓ DISPLAY, ACCEPT FROM CONSOLE
  ✓ MOVE, COMPUTE, MULTIPLY, ADD
  ✓ IF...END-IF, EVALUATE...END-EVALUATE
  ✓ PERFORM, PERFORM UNTIL...END-PERFORM
  ✓ OPEN INPUT, READ...AT END, CLOSE
  ✓ SET, STOP RUN
  
Unsupported Features (will cause error):
  ✗ CALL statements
  ✗ SORT statements  
  ✗ OCCURS clauses (arrays)
  ✗ REDEFINES clauses
  ✗ COPY statements
`);
    }
    
    private parseArgs(): { inputFile?: string; outputFile?: string; verbose?: boolean; checkOnly?: boolean; help?: boolean } {
        const args = process.argv.slice(2);
        const result: any = {};
        
        for (let i = 0; i < args.length; i++) {
            const arg = args[i];
            
            if (arg === '--help' || arg === '-h') {
                result.help = true;
            } else if (arg === '--verbose' || arg === '-v') {
                result.verbose = true;
            } else if (arg === '--check-only' || arg === '-c') {
                result.checkOnly = true;
            } else if (!result.inputFile) {
                result.inputFile = arg;
            } else if (!result.outputFile) {
                result.outputFile = arg;
            }
        }
        
        return result;
    }
    
    private validateInputFile(inputFile: string): void {
        if (!fs.existsSync(inputFile)) {
            console.error(`❌ Error: Input file not found: ${inputFile}`);
            process.exit(1);
        }
        
        const ext = path.extname(inputFile).toLowerCase();
        if (!['.cob', '.cobol', '.txt'].includes(ext)) {
            console.error(`❌ Error: Invalid file extension. Expected .cob, .cobol, or .txt, got: ${ext}`);
            process.exit(1);
        }
        
        const stats = fs.statSync(inputFile);
        if (!stats.isFile()) {
            console.error(`❌ Error: Path is not a file: ${inputFile}`);
            process.exit(1);
        }
    }
    
    private generateOutputFileName(inputFile: string): string {
        const baseName = path.basename(inputFile, path.extname(inputFile));
        const className = this.toPascalCase(baseName);
        return `${className}.java`;
    }
    
    private toPascalCase(name: string): string {
        return name
            .toLowerCase()
            .replace(/[-_]/g, ' ')
            .replace(/\b\w/g, char => char.toUpperCase())
            .replace(/\s/g, '');
    }
    
    private extractProgramId(javaCode: string): string {
        const classMatch = javaCode.match(/public class (\w+)/);
        return classMatch ? classMatch[1] : 'ConversionResult';
    }
    
    private isConversionSuccessful(javaCode: string): boolean {
        return !javaCode.includes('Conversion Status: FAILED') && 
               !javaCode.includes('public class ConversionError');
    }
    
    private extractErrorMessages(javaCode: string): string[] {
        const errors: string[] = [];
        const lines = javaCode.split('\n');
        
        for (const line of lines) {
            if (line.includes('// - ')) {
                errors.push(line.replace('// - ', '').trim());
            } else if (line.includes('System.out.println("Error:')) {
                const errorMatch = line.match(/System\.out\.println\("Error: ([^"]+)"\)/);
                if (errorMatch) {
                    errors.push(errorMatch[1]);
                }
            }
        }
        
        return errors;
    }
    
    async run(): Promise<void> {
        const args = this.parseArgs();
        
        if (args.help) {
            this.showHelp();
            return;
        }
        
        if (!args.inputFile) {
            console.error('❌ Error: No input file specified');
            console.error('Use --help for usage information');
            process.exit(1);
        }
        
        try {
            // Validate input
            this.validateInputFile(args.inputFile);
            
            if (args.verbose) {
                console.log(`📝 Reading COBOL file: ${args.inputFile}`);
            }
            
            // Read COBOL source
            const cobolSource = fs.readFileSync(args.inputFile, 'utf-8');
            
            if (args.verbose) {
                console.log(`📏 File size: ${cobolSource.length} characters`);
                console.log(`📄 Lines: ${cobolSource.split('\\n').length}`);
            }
            
            // Convert to Java
            if (args.verbose) {
                console.log('🔄 Starting conversion...');
            }
            
            const javaCode = this.converter.convert(cobolSource);
            const isSuccess = this.isConversionSuccessful(javaCode);
            
            if (isSuccess) {
                console.log('✅ Conversion successful!');
                
                if (args.verbose) {
                    const programId = this.extractProgramId(javaCode);
                    console.log(`📦 Generated class: ${programId}`);
                    console.log(`📊 Java code size: ${javaCode.length} characters`);
                }
                
                if (!args.checkOnly) {
                    // Generate output file name
                    const outputFile = args.outputFile || this.generateOutputFileName(args.inputFile);
                    
                    // Write Java code
                    fs.writeFileSync(outputFile, javaCode, 'utf-8');
                    console.log(`💾 Java file written: ${outputFile}`);
                    
                    if (args.verbose) {
                        console.log(`
🎯 Next steps:
   1. Compile: javac ${outputFile}
   2. Run: java ${this.extractProgramId(javaCode)}
   3. Ensure /data directory exists for file operations
`);
                    }
                }
            } else {
                console.log('❌ Conversion failed!');
                
                const errors = this.extractErrorMessages(javaCode);
                console.log('\\n🚫 Unsupported features found:');
                
                for (const error of errors) {
                    console.log(`   • ${error}`);
                }
                
                console.log(`
💡 Recommendations:
   • Remove or replace unsupported COBOL features
   • Use only basic COBOL statements (DISPLAY, ACCEPT, MOVE, IF, PERFORM)
   • Check the supported features list with --help
`);
                
                if (!args.checkOnly) {
                    // Still write the error report for reference
                    const outputFile = args.outputFile || this.generateOutputFileName(args.inputFile);
                    fs.writeFileSync(outputFile, javaCode, 'utf-8');
                    console.log(`📋 Error report written: ${outputFile}`);
                }
                
                process.exit(1);
            }
            
        } catch (error) {
            console.error(`💥 Fatal error: ${error}`);
            if (args.verbose) {
                console.error(error);
            }
            process.exit(1);
        }
    }
}

// Run CLI if this file is executed directly
if (require.main === module) {
    const cli = new CobolConverterCLI();
    cli.run().catch(error => {
        console.error('💥 Unexpected error:', error);
        process.exit(1);
    });
}

export { CobolConverterCLI };