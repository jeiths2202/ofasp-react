package com.openasp.smed;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.openasp.common.JSONResponse;
import com.openasp.encoding.EncodingService;
import com.openasp.websocket.PositionSmedWebSocketService;

/**
 * Generated Java class from COBOL program
 * Supports position-based SMED rendering with WebSocket communication
 * Auto-generated on 2025-07-31T15:28:49.371350
 */
@Component
public class TestProgram {

    private static final PositionField[] MAP_DEFINITION;
    private PositionSmedWebSocketService webSocketService;
    private EncodingService encodingService;
    private Map<String, String> currentFieldData = new ConcurrentHashMap<>();

    static {
        MAP_DEFINITION = new PositionField[] {
            new PositionField(1, 1, 20),
            new PositionField(1, 23, 10),
            new PositionField(1, 35, 15),
            new PositionField(1, 52, 8),
            new PositionField(1, 62, 20)
        };
    }

    public void displayScreen-Data() {
        try {
            // Prepare field data array matching position order
            String[] fieldData = new String[MAP_DEFINITION.length];
            
            // Set display field values
            fieldData[0] = "Test Screen";
            fieldData[1] = getCurrentFieldValue("INPUT-FIELD");
            fieldData[2] = getCurrentFieldValue("OUTPUT-FIELD");
            
            // Convert to UTF-8 if needed
            String[] utf8FieldData = convertToUtf8(fieldData);
            
            // Send position-based SMED display via WebSocket
            Map<String, Object> displayData = new HashMap<>();
            displayData.put("map_name", "SCREEN-DATA");
            displayData.put("map_data", Arrays.asList(MAP_DEFINITION));
            displayData.put("field_data", Arrays.asList(utf8FieldData));
            displayData.put("terminal_id", getCurrentTerminalId());
            displayData.put("encoding", "utf-8");
            displayData.put("timestamp", new Date().toInstant().toString());
            
            webSocketService.sendPositionSmedDisplay(displayData);
            
            // Handle interactive processing if DESTINATION specified
            // No interactive processing required
            
            logger.info("Display sent for SCREEN-DATA");
            
        } catch (Exception e) {
            logger.error("Error in display method: " + e.getMessage(), e);
            throw new RuntimeException("Display operation failed", e);
        }
        
    }

    public Map<String, String> acceptUser-Input() {
        try {
            // Setup WebSocket listener for input events  
            String mapName = "USER-INPUT";
            
            // Subscribe to position SMED updates
            webSocketService.subscribeToPositionUpdates(mapName, (updateData) -> {
                handlePositionUpdate(updateData);
            });
            
            // Setup key event handler
            String[] terminationKeysArray = ["ENTER", "F3", "F12"];
            webSocketService.subscribeToKeyEvents(mapName, (keyEvent) -> {
                return handleKeyEvent(keyEvent, terminationKeysArray);
            });
            
            // Wait for user input or termination key
            CompletableFuture<Map<String, String>> inputFuture = new CompletableFuture<>();
            
            // Setup termination key handling
            setupTerminationKeys(terminationKeysArray, inputFuture);
            
            // Block until input is complete
            Map<String, String> inputData = inputFuture.get(300, TimeUnit.SECONDS); // 5 minute timeout
            
            // Convert from UTF-8 to SJIS if needed
            Map<String, String> sjisInputData = convertFromUtf8(inputData);
            
            // Update current field data
            currentFieldData.putAll(sjisInputData);
            
            logger.info("Accept completed for USER-INPUT");
            return sjisInputData;
            
        } catch (TimeoutException e) {
            logger.warn("Accept operation timed out for USER-INPUT");
            throw new RuntimeException("Input timeout", e);
        } catch (Exception e) {
            logger.error("Error in accept method: " + e.getMessage(), e);
            throw new RuntimeException("Accept operation failed", e);
        }
        
    }

    public String[] convertToUtf8(String[] sjisData) {
            try {
                String[] utf8Data = new String[sjisData.length];
                for (int i = 0; i < sjisData.length; i++) {
                    if (sjisData[i] != null) {
                        utf8Data[i] = encodingService.convertSjisToUtf8(sjisData[i]);
                    } else {
                        utf8Data[i] = "";
                    }
                }
                return utf8Data;
            } catch (Exception e) {
                logger.error("UTF-8 conversion error: " + e.getMessage(), e);
                return sjisData; // Return original data on error
            }
            
    }

    public Map<String, String> convertFromUtf8(Map<String, String> utf8Data) {
            try {
                Map<String, String> sjisData = new HashMap<>();
                for (Map.Entry<String, String> entry : utf8Data.entrySet()) {
                    String convertedValue = encodingService.convertUtf8ToSjis(entry.getValue());
                    sjisData.put(entry.getKey(), convertedValue);
                }
                return sjisData;
            } catch (Exception e) {
                logger.error("SJIS conversion error: " + e.getMessage(), e);
                return utf8Data; // Return original data on error
            }
            
    }

    // Helper methods
    private String getCurrentTerminalId() {
        // Get current terminal ID from session or context
        return "TERM001"; // Default terminal ID
    }
    
    private String getCurrentFieldValue(String fieldName) {
        return currentFieldData.getOrDefault(fieldName, "");
    }
    
    private boolean isInteractiveProcessingActive() {
        // Check if interactive processing should continue
        return webSocketService.isInteractiveMode(getCurrentTerminalId());
    }
    
    private void processWebSocketEvents() {
        // Process incoming WebSocket events
        webSocketService.processIncomingEvents(getCurrentTerminalId());
    }
    
    private void handlePositionUpdate(Map<String, Object> updateData) {
        // Handle position-based field updates from WebSocket
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> updates = (List<Map<String, Object>>) updateData.get("updates");
        
        for (Map<String, Object> update : updates) {
            int row = (Integer) update.get("row");
            int col = (Integer) update.get("col");
            String value = (String) update.get("value");
            
            // Find field by position and update value
            updateFieldByPosition(row, col, value);
        }
    }
    
    private boolean handleKeyEvent(Map<String, Object> keyEvent, String[] terminationKeys) {
        String key = (String) keyEvent.get("key");
        
        // Check if it's a termination key
        for (String termKey : terminationKeys) {
            if (termKey.equals(key)) {
                return true; // Terminate input
            }
        }
        
        return false; // Continue input
    }
    
    private void setupTerminationKeys(String[] terminationKeys, 
                                    CompletableFuture<Map<String, String>> inputFuture) {
        // Setup handlers for termination keys
        for (String key : terminationKeys) {
            webSocketService.onKeyEvent(getCurrentTerminalId(), key, () -> {
                inputFuture.complete(getCurrentFieldData());
            });
        }
    }
    
    private Map<String, String> getCurrentFieldData() {
        return new HashMap<>(currentFieldData);
    }
    
    private void updateFieldByPosition(int row, int col, String value) {
        // Find field by position in MAP_DEFINITION
        for (int i = 0; i < MAP_DEFINITION.length; i++) {
            PositionField field = MAP_DEFINITION[i];
            if (field.row == row && field.col == col) {
                currentFieldData.put("field_" + i, value);
                break;
            }
        }
    }
}

/**
 * Position field definition for SMED mapping
 */
class PositionField {
    public final int row;
    public final int col;
    public final int length;
    
    public PositionField(int row, int col, int length) {
        this.row = row;
        this.col = col; 
        this.length = length;
    }
}
