package com.openasp.cobol;

import java.util.Map;
import java.util.function.Consumer;

/**
 * WebSocketService Interface
 * 
 * Provides WebSocket-based interactive display processing
 * Handles position-based SMED rendering for display files
 */
public interface WebSocketService {
    
    /**
     * Enable interactive mode for a terminal
     * 
     * @param terminalId Terminal identifier
     * @throws Exception if initialization fails
     */
    void enableInteractiveMode(String terminalId) throws Exception;
    
    /**
     * Disable interactive mode for a terminal
     * 
     * @param terminalId Terminal identifier
     * @throws Exception if cleanup fails
     */
    void disableInteractiveMode(String terminalId) throws Exception;
    
    /**
     * Send position-based SMED display data
     * 
     * @param displayData Map containing display information
     * @throws Exception if send fails
     */
    void sendPositionSmedDisplay(Map<String, Object> displayData) throws Exception;
    
    /**
     * Subscribe to position updates from client
     * 
     * @param formatName Display format name
     * @param updateHandler Callback to handle updates
     * @throws Exception if subscription fails
     */
    void subscribeToPositionUpdates(String formatName, Consumer<Map<String, Object>> updateHandler) throws Exception;
    
    /**
     * Unsubscribe from position updates
     * 
     * @param formatName Display format name
     * @throws Exception if unsubscription fails
     */
    void unsubscribeFromPositionUpdates(String formatName) throws Exception;
    
    /**
     * Send error message to terminal
     * 
     * @param terminalId Terminal identifier
     * @param message Error message
     * @throws Exception if send fails
     */
    void sendError(String terminalId, String message) throws Exception;
    
    /**
     * Check if terminal is connected
     * 
     * @param terminalId Terminal identifier
     * @return true if connected, false otherwise
     */
    boolean isTerminalConnected(String terminalId);
}