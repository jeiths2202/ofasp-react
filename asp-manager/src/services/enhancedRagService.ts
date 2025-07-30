// 개선된 RAG 서비스 구현
import { DocumentChunk, SearchResult, RAGResponse } from './ragService';

// 불용어 목록 (한국어/일본어)
const STOP_WORDS = {
  ko: new Set(['은', '는', '이', '가', '을', '를', '에', '에서', '로', '으로', '의', '와', '과', '하고', '그리고', '하지만', '그러나']),
  ja: new Set(['は', 'が', 'を', 'に', 'で', 'から', 'まで', 'と', 'の', 'や', 'も', 'だ', 'である', 'です', 'ます'])
};

// TF-IDF 계산을 위한 클래스
class TFIDFCalculator {
  private documentCount: number = 0;
  private termDocumentFreq: Map<string, number> = new Map();
  
  // 문서 집합 분석
  analyzeDocuments(documents: DocumentChunk[]): void {
    this.documentCount = documents.length;
    this.termDocumentFreq.clear();
    
    documents.forEach(doc => {
      const terms = this.extractTerms(doc.content, doc.metadata.language);
      const uniqueTerms = new Set(terms);
      
      uniqueTerms.forEach(term => {
        const currentCount = this.termDocumentFreq.get(term) || 0;
        this.termDocumentFreq.set(term, currentCount + 1);
      });
    });
  }
  
  // 텍스트에서 용어 추출
  private extractTerms(text: string, language: 'ko' | 'ja'): string[] {
    const stopWords = STOP_WORDS[language] || new Set();
    
    // 텍스트 정규화
    const normalized = text.toLowerCase()
      .replace(/[^\p{L}\p{N}\s]/gu, ' ') // 특수문자 제거
      .replace(/\s+/g, ' ')
      .trim();
    
    // 단어 분할 및 불용어 제거
    return normalized.split(' ')
      .filter(word => word.length > 1 && !stopWords.has(word));
  }
  
  // TF-IDF 스코어 계산
  calculateTFIDF(query: string, document: DocumentChunk): number {
    const queryTerms = this.extractTerms(query, document.metadata.language);
    const docTerms = this.extractTerms(document.content, document.metadata.language);
    
    if (queryTerms.length === 0 || docTerms.length === 0) return 0;
    
    let score = 0;
    const docTermCount = new Map<string, number>();
    
    // 문서 내 용어 빈도 계산
    docTerms.forEach(term => {
      const count = docTermCount.get(term) || 0;
      docTermCount.set(term, count + 1);
    });
    
    queryTerms.forEach(queryTerm => {
      const tf = (docTermCount.get(queryTerm) || 0) / docTerms.length;
      const df = this.termDocumentFreq.get(queryTerm) || 0;
      const idf = df > 0 ? Math.log(this.documentCount / df) : 0;
      
      score += tf * idf;
    });
    
    return score;
  }
}

// 개선된 언어 감지
class LanguageDetector {
  static detectLanguage(text: string): 'ko' | 'ja' {
    // 한국어 문자 패턴
    const koreanChars = (text.match(/[가-힣ㄱ-ㅎㅏ-ㅣ]/g) || []).length;
    const koreanWords = (text.match(/\b[가-힣]+\b/g) || []).length;
    
    // 일본어 문자 패턴  
    const hiragana = (text.match(/[ひらがな]/g) || []).length;
    const katakana = (text.match(/[カタカナ]/g) || []).length;
    const japaneseChars = hiragana + katakana;
    const japaneseWords = (text.match(/[ひらがなカタカナ一-龯]+/g) || []).length;
    
    // 점수 계산
    const koreanScore = koreanChars * 2 + koreanWords * 3;
    const japaneseScore = japaneseChars * 2 + japaneseWords * 3;
    
    // 임계값 기반 판정
    if (koreanScore > japaneseScore && koreanScore > 5) return 'ko';
    if (japaneseScore > koreanScore && japaneseScore > 5) return 'ja';
    
    // 기본값 (한국어 우선)
    return 'ko';
  }
  
  // 다국어 혼재 텍스트 처리
  static analyzeMultilingual(text: string): { primary: 'ko' | 'ja', confidence: number } {
    const korean = (text.match(/[가-힣]/g) || []).length;
    const japanese = (text.match(/[ひらがなカタカナ]/g) || []).length;
    const total = korean + japanese;
    
    if (total === 0) return { primary: 'ko', confidence: 0 };
    
    const koreanRatio = korean / total;
    const japaneseRatio = japanese / total;
    
    if (koreanRatio > japaneseRatio) {
      return { primary: 'ko', confidence: koreanRatio };
    } else {
      return { primary: 'ja', confidence: japaneseRatio };
    }
  }
}

// N-gram 기반 유사도 계산
class NGramSimilarity {
  static calculateSimilarity(text1: string, text2: string, n: number = 2): number {
    const ngrams1 = this.generateNGrams(text1, n);
    const ngrams2 = this.generateNGrams(text2, n);
    
    if (ngrams1.length === 0 && ngrams2.length === 0) return 1;
    if (ngrams1.length === 0 || ngrams2.length === 0) return 0;
    
    const set1 = new Set(ngrams1);
    const set2 = new Set(ngrams2);
    
    // Jaccard 유사도 계산
    const intersection = new Set(Array.from(set1).filter(x => set2.has(x)));
    const union = new Set([...Array.from(set1), ...Array.from(set2)]);
    
    return intersection.size / union.size;
  }
  
  private static generateNGrams(text: string, n: number): string[] {
    const normalized = text.toLowerCase().replace(/\s+/g, '');
    const ngrams: string[] = [];
    
    for (let i = 0; i <= normalized.length - n; i++) {
      ngrams.push(normalized.substr(i, n));
    }
    
    return ngrams;
  }
}

// 개선된 RAG 서비스 클래스
export class EnhancedRAGService {
  private documents: DocumentChunk[] = [];
  private tfidfCalculator: TFIDFCalculator = new TFIDFCalculator();
  private initialized = false;
  
  async initialize(documents: DocumentChunk[]): Promise<void> {
    if (this.initialized) return;
    
    this.documents = documents;
    this.tfidfCalculator.analyzeDocuments(documents);
    this.initialized = true;
    
    console.log('Enhanced RAG Service initialized with', documents.length, 'document chunks');
  }
  
  // 개선된 문서 검색
  async searchDocuments(query: string, language: 'ko' | 'ja', limit: number = 5): Promise<SearchResult[]> {
    if (!this.initialized) {
      throw new Error('Service not initialized');
    }
    
    const results: SearchResult[] = [];
    
    this.documents.forEach(doc => {
      // 다중 유사도 점수 계산
      const tfidfScore = this.tfidfCalculator.calculateTFIDF(query, doc);
      const ngramScore = NGramSimilarity.calculateSimilarity(query, doc.content);
      const keywordScore = this.calculateKeywordSimilarity(query, doc.content);
      
      // 언어 일치 보너스
      const languageBonus = doc.metadata.language === language ? 1.2 : 1.0;
      
      // 가중 평균 점수
      const combinedScore = (
        tfidfScore * 0.4 + 
        ngramScore * 0.3 + 
        keywordScore * 0.3
      ) * languageBonus;
      
      if (combinedScore > 0.1) { // 최소 임계값
        results.push({
          chunk: doc,
          score: combinedScore
        });
      }
    });
    
    // 점수 기준 정렬 후 상위 결과 반환
    return results
      .sort((a, b) => b.score - a.score)
      .slice(0, limit);
  }
  
  // 키워드 기반 유사도 (기존 방식 개선)
  private calculateKeywordSimilarity(query: string, content: string): number {
    const queryWords = query.toLowerCase().split(/\s+/);
    const contentWords = content.toLowerCase().split(/\s+/);
    
    let matches = 0;
    let totalWords = queryWords.length;
    
    queryWords.forEach(queryWord => {
      if (contentWords.some(contentWord => 
        contentWord.includes(queryWord) || queryWord.includes(contentWord)
      )) {
        matches++;
      }
    });
    
    return matches / totalWords;
  }
  
  // 개선된 응답 생성
  async generateResponse(query: string, language: 'ko' | 'ja'): Promise<RAGResponse> {
    // 언어 자동 감지 (요청 언어와 비교)
    const detectedLang = LanguageDetector.detectLanguage(query);
    const finalLanguage = language || detectedLang;
    
    const searchResults = await this.searchDocuments(query, finalLanguage);
    
    if (searchResults.length === 0) {
      const noResultsMessage = finalLanguage === 'ko' 
        ? '죄송합니다. 해당 질문에 대한 정보를 ASP 매뉴얼에서 찾을 수 없습니다. 다른 키워드로 다시 시도해보시거나, 더 구체적인 질문을 해주세요.'
        : '申し訳ございません。その質問に関する情報をASPマニュアルで見つけることができませんでした。他のキーワードで再度お試しいただくか、より具体的な質問をしてください。';
      
      return {
        answer: noResultsMessage,
        sources: [],
        language: finalLanguage
      };
    }
    
    // 컨텍스트 기반 응답 생성
    const contextChunks = searchResults.slice(0, 3); // 상위 3개 결과 사용
    const contextText = contextChunks.map(result => result.chunk.content).join('\\n\\n');
    
    // 응답 템플릿 (언어별)
    const responseTemplate = this.generateContextualResponse(query, contextText, finalLanguage, searchResults);
    
    return {
      answer: responseTemplate,
      sources: searchResults,
      language: finalLanguage
    };
  }
  
  // 컨텍스트 기반 응답 생성
  private generateContextualResponse(
    query: string, 
    context: string, 
    language: 'ko' | 'ja', 
    sources: SearchResult[]
  ): string {
    const intro = language === 'ko'
      ? `질문: "${query}"\\n\\nASP 매뉴얼을 검색한 결과, 다음 정보를 찾았습니다:\\n\\n`
      : `質問: "${query}"\\n\\nASPマニュアルを検索した結果、以下の情報が見つかりました：\\n\\n`;
    
    // 관련 내용 요약
    const relevantContent = this.summarizeContent(context, language);
    
    // 출처 정보
    const sourcesInfo = language === 'ko'
      ? `\\n\\n📚 참고 문서 (관련도 순):\\n${sources.map((result, index) => 
          `${index + 1}. ${result.chunk.metadata.source} - ${result.chunk.metadata.section} (일치도: ${(result.score * 100).toFixed(1)}%)`
        ).join('\\n')}`
      : `\\n\\n📚 参考文書（関連度順）：\\n${sources.map((result, index) => 
          `${index + 1}. ${result.chunk.metadata.source} - ${result.chunk.metadata.section} (一致度: ${(result.score * 100).toFixed(1)}%)`
        ).join('\\n')}`;
    
    const footer = language === 'ko'
      ? '\\n\\n💡 더 자세한 정보가 필요하시면 구체적인 키워드나 상황을 포함하여 다시 질문해주세요.'
      : '\\n\\n💡 さらに詳しい情報が必要でしたら、具体的なキーワードや状況を含めて再度ご質問ください。';
    
    return intro + relevantContent + sourcesInfo + footer;
  }
  
  // 내용 요약 생성
  private summarizeContent(content: string, language: 'ko' | 'ja'): string {
    // 간단한 요약 로직 (실제로는 LLM API 사용 권장)
    const sentences = content.split(/[.。]/).filter(s => s.trim().length > 10);
    const maxSentences = Math.min(5, sentences.length);
    
    return sentences.slice(0, maxSentences).join(language === 'ko' ? '. ' : '。') + 
           (language === 'ko' ? '.' : '。');
  }
}

// 기본 export
export const enhancedRagService = new EnhancedRAGService();