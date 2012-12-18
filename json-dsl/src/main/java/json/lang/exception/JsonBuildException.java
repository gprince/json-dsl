package json.lang.exception;

/**
 * An exception that can be raised in builders
 * 
 * @author gprince
 * 
 */
public class JsonBuildException extends Exception {

    /**
     * Generated serialVersionUID
     */
    private static final long serialVersionUID = 3639707553546120548L;

    /**
     * Constructs a new <code>JsonBuildException</code> with the specified
     * detail message
     * 
     * @param message
     *            the detail message
     */
    public JsonBuildException(String message) {
        super(message);
    }

    /**
     * Constructs a new <code>JsonBuildException</code> with the specified
     * detail message and cause
     * 
     * @param message
     *            the detail message
     * @param cause
     *            the cause
     */
    public JsonBuildException(String message, Throwable cause) {
        super(message, cause);
    }
}