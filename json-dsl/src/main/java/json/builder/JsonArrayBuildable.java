package json.builder;

import json.lang.JsonValue.JsonArray;

/**
 * 
 * 
 * @author gprince
 * 
 */
public interface JsonArrayBuildable extends JsonBuildable<JsonArray>,
		JsonArrayAppender<JsonArrayBuildable> {

}