package json.builder;

import json.lang.JsonValue.JsonObject;

/**
 * 
 * 
 * @author gprince
 * 
 */
public interface JsonObjectBuildable extends JsonBuildable<JsonObject>,
		JsonObjectAppender<JsonObjectBuildable> {

}