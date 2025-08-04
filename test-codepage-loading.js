// Simple test to verify codepage loading from browser
(async function testCodePageLoading() {
  console.log('Testing codepage file loading...');
  
  const codePageFiles = [
    'EBCASCUS.txt',
    'EBCASCJP.txt', 
    'JEFASCK.txt',
    'KEISASCK.txt',
    'ASCEBCUS.txt',
    'ASCEBCJP.txt',
    'ASCJEFK.txt',
    'ASCJEISK.txt'
  ];
  
  let allSuccessful = true;
  
  for (const file of codePageFiles) {
    try {
      console.log(`Testing ${file}...`);
      const response = await fetch(`/codepages/${file}`);
      
      if (response.ok) {
        const content = await response.text();
        const lines = content.split('\n');
        console.log(`✅ ${file}: ${response.status} ${response.statusText} - ${lines.length} lines`);
        
        // Check for expected content
        if (content.includes('[Single byte mapping table]')) {
          console.log(`  ✅ Contains single byte mapping table`);
        }
        if (content.includes('00 - 00')) {
          console.log(`  ✅ Contains mapping entries`);
        }
      } else {
        console.error(`❌ ${file}: ${response.status} ${response.statusText}`);
        allSuccessful = false;
      }
    } catch (error) {
      console.error(`❌ ${file}: Error - ${error.message}`);
      allSuccessful = false;
    }
  }
  
  if (allSuccessful) {
    console.log('🎉 All codepage files loaded successfully!');
    console.log('The EBCDIC conversion functionality should now work properly.');
  } else {
    console.error('❌ Some codepage files failed to load.');
  }
})();