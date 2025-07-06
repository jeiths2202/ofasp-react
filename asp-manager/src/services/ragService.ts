export interface DocumentChunk {
  id: string;
  content: string;
  metadata: {
    source: string;
    page?: number;
    section?: string;
    language: 'ko' | 'ja';
  };
  embedding?: number[];
}

export interface SearchResult {
  chunk: DocumentChunk;
  score: number;
}

export interface RAGResponse {
  answer: string;
  sources: SearchResult[];
  language: 'ko' | 'ja';
}

class RAGService {
  private documents: DocumentChunk[] = [];
  private initialized = false;

  // Initialize with empty documents array - real documents loaded via advancedRAGSystem
  async initialize(): Promise<void> {
    if (this.initialized) return;

    // 기본 RAG 서비스는 빈 배열로 초기화
    // advancedRAGSystem이 실제 파일 시스템에서 문서를 로드함
    this.documents = [];
    
    this.initialized = true;
    console.log('⚠️ 기본 RAG 서비스 초기화 완료 (빈 문서 배열) - advancedRAGSystem 사용 권장');
  }

  // Simple text similarity scoring (in production, use vector embeddings)
  private calculateSimilarity(query: string, content: string): number {
    const queryWords = query.toLowerCase().split(/\s+/);
    const contentWords = content.toLowerCase().split(/\s+/);
    
    let matches = 0;
    queryWords.forEach(queryWord => {
      if (contentWords.some(contentWord => 
        contentWord.includes(queryWord) || queryWord.includes(contentWord)
      )) {
        matches++;
      }
    });
    
    return matches / queryWords.length;
  }

  // Search documents based on query
  async searchDocuments(query: string, language: 'ko' | 'ja'): Promise<SearchResult[]> {
    await this.initialize();

    const results: SearchResult[] = [];

    this.documents.forEach(doc => {
      const score = this.calculateSimilarity(query, doc.content);
      if (score > 0.1) { // Minimum relevance threshold
        results.push({
          chunk: doc,
          score: score
        });
      }
    });

    // Sort by relevance score
    results.sort((a, b) => b.score - a.score);

    // Return top 3 most relevant results
    return results.slice(0, 3);
  }

  // Generate response based on search results
  async generateResponse(query: string, language: 'ko' | 'ja'): Promise<RAGResponse> {
    const searchResults = await this.searchDocuments(query, language);

    if (searchResults.length === 0) {
      const noResultsMessage = language === 'ko' 
        ? '죄송합니다. 해당 질문에 대한 정보를 ASP 매뉴얼에서 찾을 수 없습니다. 다른 키워드로 다시 시도해보시거나, 더 구체적인 질문을 해주세요.'
        : '申し訳ございません。その質問に関する情報をASPマニュアルで見つけることができませんでした。他のキーワードで再度お試しいただくか、より具体的な質問をしてください。';
      
      return {
        answer: noResultsMessage,
        sources: [],
        language: language
      };
    }

    // Create response based on found documents
    const relevantContent = searchResults.map(result => result.chunk.content).join('\n\n');
    
    const responseIntro = language === 'ko'
      ? 'ASP 매뉴얼을 검색한 결과, 다음 정보를 찾았습니다:\n\n'
      : 'ASPマニュアルを検索した結果、以下の情報が見つかりました：\n\n';

    const sourcesInfo = language === 'ko'
      ? `\n\n📚 참고 문서:\n${searchResults.map((result, index) => 
          `${index + 1}. ${result.chunk.metadata.source} (${result.chunk.metadata.section})`
        ).join('\n')}`
      : `\n\n📚 参考文書：\n${searchResults.map((result, index) => 
          `${index + 1}. ${result.chunk.metadata.source} (${result.chunk.metadata.section})`
        ).join('\n')}`;

    const answer = responseIntro + relevantContent + sourcesInfo;

    return {
      answer: answer,
      sources: searchResults,
      language: language
    };
  }

  // Add new document to the collection
  async addDocument(content: string, metadata: Omit<DocumentChunk['metadata'], 'language'>, language: 'ko' | 'ja'): Promise<void> {
    const newDoc: DocumentChunk = {
      id: Date.now().toString(),
      content: content,
      metadata: {
        ...metadata,
        language: language
      }
    };

    this.documents.push(newDoc);
    console.log('Document added:', newDoc.id);
  }

  // Get all documents (for admin purposes)
  async getAllDocuments(): Promise<DocumentChunk[]> {
    await this.initialize();
    return this.documents;
  }
}

// Export singleton instance
export const ragService = new RAGService();