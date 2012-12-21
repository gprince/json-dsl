package json.builder;

import json.lang.exception.JsonBuildException;

/**
 * Contrat pour un builder permettant de construire un
 * {@link json.lang.JsonValue.JsonObject}
 * 
 * @author gprince
 * 
 * @param <T>
 *            Type de builder retourné à la fin de la construction d'une
 *            JsonStructure
 */
public interface JsonObjectBuilder<T extends JsonBuilder> extends
		JsonObjectAppender<JsonObjectBuilder<T>>, JsonBuilder {

	/**
	 * Construit le JsonObject en cours de construction, rajoute le au builder
	 * parent et retourne ce dernier
	 * 
	 * @return le JsonBuilder parent
	 * @throws JsonBuildException
	 */
	T endObject() throws JsonBuildException;
}
