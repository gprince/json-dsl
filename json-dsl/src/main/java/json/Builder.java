package json;

import java.util.List;

/**
 * @author Gregory
 *
 * @param <T>
 */
public interface Builder<T> {

    List<T> getStructure();
}