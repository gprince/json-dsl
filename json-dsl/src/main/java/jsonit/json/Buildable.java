package jsonit.json;

import jsonit.json.JsonValue.JsonStructure;

public interface Buildable<T extends JsonStructure<?>> {

    T build();
}