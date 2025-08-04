const fs = require('fs');
const pdfParse = require('pdf-parse');

async function extractPDF() {
  try {
    const pdfPath = '/data/asp-manuals/マニュアル/システムコマンド集_system_command_J2K0-6160-01.pdf';
    const dataBuffer = fs.readFileSync(pdfPath);
    
    console.log('PDFファイルを読み込み中...');
    const data = await pdfParse(dataBuffer);
    
    console.log('=== PDF メタ情報 ===');
    console.log(`ページ数: ${data.numpages}`);
    console.log(`タイトル: ${data.info?.Title || 'N/A'}`);
    console.log(`作成者: ${data.info?.Creator || 'N/A'}`);
    console.log('\n=== PDF 内容 (最初の10000文字) ===\n');
    console.log(data.text.substring(0, 10000));
    
    // 主要なセクションを探す
    console.log('\n=== 主要セクション ===');
    const lines = data.text.split('\n');
    const sections = [];
    
    lines.forEach((line, index) => {
      // 章タイトルやコマンド名を検出
      if (line.match(/^第\d+章|^\d+\.|^[A-Z]+コマンド|^●/)) {
        sections.push({ line: line.trim(), index });
      }
    });
    
    sections.slice(0, 50).forEach(section => {
      console.log(`- ${section.line}`);
    });
    
  } catch (error) {
    console.error('エラー:', error.message);
  }
}

extractPDF();