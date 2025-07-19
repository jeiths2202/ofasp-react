// EBCDIC 변환 테스트 스크립트
const { convertEbcdicToAscii } = require('./src/utils/ebcdicConverter');

// 테스트 데이터 (사용자가 제공한 것)
const testInput = "4040404040404040404040C4C9E2D7D3C1E8407D0E60FA4660FA8D61428660FA8D667C457F488D60F9805573657260F9802041757468656E7469636174696F6E272E2020202020202020202020202020";

console.log('=== EBCDIC 변환 테스트 ===');
console.log('입력 헥스:', testInput);
console.log('');

// SOSI 사용 안함
console.log('1. SOSI 사용 안함:');
const result1 = convertEbcdicToAscii(testInput, {
  useSOSI: false,
  debugCallback: (msg) => console.log('  ' + msg)
});
console.log('출력:', `"${result1.output}"`);
console.log('오류:', result1.errors);
console.log('');

// SOSI 사용 (제거)
console.log('2. SOSI 사용 (제거):');
const result2 = convertEbcdicToAscii(testInput, {
  useSOSI: true,
  sosiHandling: 'remove',
  debugCallback: (msg) => console.log('  ' + msg)
});
console.log('출력:', `"${result2.output}"`);
console.log('오류:', result2.errors);
console.log('');

// SOSI 사용 (공백 변환)
console.log('3. SOSI 사용 (공백 변환):');
const result3 = convertEbcdicToAscii(testInput, {
  useSOSI: true,
  sosiHandling: 'space',
  debugCallback: (msg) => console.log('  ' + msg)
});
console.log('출력:', `"${result3.output}"`);
console.log('오류:', result3.errors);
console.log('');