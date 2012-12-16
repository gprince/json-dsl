package jsonit.json;

public interface Buildable<T extends JsonValue<?>> {

    T build();
}