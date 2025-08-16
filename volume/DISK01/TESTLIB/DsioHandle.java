package com.openasp.cobol;

/**
 * DsioHandle - Dataset I/O Handle
 * 
 * Represents an open dataset with lock information
 * Maps to dsio_t structure in C library
 */
public class DsioHandle {
    private long nativeHandle;  // Pointer to native dsio_t structure
    private String dataset;     // Dataset name
    private String level;       // Lock level (SHR/OLD/MOD)
    private String recfm;       // Record format (FB/VB)
    private int lrecl;          // Logical record length
    private boolean ownsLock;   // Whether this handle owns the lock
    private boolean closed;     // Whether handle is closed
    
    // Constructor
    public DsioHandle(long nativeHandle, String dataset, String level) {
        this.nativeHandle = nativeHandle;
        this.dataset = dataset;
        this.level = level;
        this.closed = false;
        this.ownsLock = true;
    }
    
    // Getters
    public long getNativeHandle() {
        return nativeHandle;
    }
    
    public String getDataset() {
        return dataset;
    }
    
    public String getLevel() {
        return level;
    }
    
    public String getRecfm() {
        return recfm;
    }
    
    public void setRecfm(String recfm) {
        this.recfm = recfm;
    }
    
    public int getLrecl() {
        return lrecl;
    }
    
    public void setLrecl(int lrecl) {
        this.lrecl = lrecl;
    }
    
    public boolean isOwnsLock() {
        return ownsLock;
    }
    
    public boolean isClosed() {
        return closed;
    }
    
    public void setClosed(boolean closed) {
        this.closed = closed;
    }
    
    @Override
    public String toString() {
        return "DsioHandle{" +
                "dataset='" + dataset + '\'' +
                ", level='" + level + '\'' +
                ", recfm='" + recfm + '\'' +
                ", lrecl=" + lrecl +
                ", closed=" + closed +
                '}';
    }
}