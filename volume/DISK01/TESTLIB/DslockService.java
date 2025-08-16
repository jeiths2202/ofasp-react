package com.openasp.cobol;

/**
 * DslockService Interface
 * 
 * Provides Java wrapper for dslock_suite C library
 * All file I/O operations must use this service to ensure data integrity
 */
public interface DslockService {
    
    /**
     * Open a dataset with appropriate lock level
     * 
     * @param dataset Dataset name (e.g., "DISK01/TESTLIB/EMPLOYEE.FB")
     * @param mode Access mode ("READ", "WRITE", "READWRITE")
     * @return Handle to the opened dataset
     * @throws Exception if open fails
     */
    DsioHandle openDataset(String dataset, String mode) throws Exception;
    
    /**
     * Read a record from the dataset
     * 
     * @param handle Dataset handle
     * @return Record data as byte array, null if EOF
     * @throws Exception if read fails
     */
    byte[] readRecord(DsioHandle handle) throws Exception;
    
    /**
     * Write a record to the dataset
     * 
     * @param handle Dataset handle
     * @param data Record data
     * @throws Exception if write fails
     */
    void writeRecord(DsioHandle handle, byte[] data) throws Exception;
    
    /**
     * Close the dataset and release lock
     * 
     * @param handle Dataset handle
     * @throws Exception if close fails
     */
    void closeDataset(DsioHandle handle) throws Exception;
    
    /**
     * Get current lock status for a dataset
     * 
     * @param dataset Dataset name
     * @return Lock status information
     * @throws Exception if status query fails
     */
    String getLockStatus(String dataset) throws Exception;
}