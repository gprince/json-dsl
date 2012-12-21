package json.builder;

import java.math.BigDecimal;

import json.lang.JsonValue.JsonArray;
import json.lang.JsonValue.JsonObject;
import json.lang.exception.JsonBuildException;

/**
 * @author gprince
 * 
 * @param <T>
 *            Type de builder retourn� � la fin de la construction d'une
 *            JsonStructure
 */
public interface JsonArrayAppender<T extends JsonBuilder> {

	/**
	 * Commence un nouveau JsonArray � rajouter au JsonArray en cours de
	 * construction
	 * 
	 * @return un nouveau builder pour le nouveau JsonArray � construire et �
	 *         rajouter
	 * @throws JsonBuildException
	 */
	JsonArrayBuilder<T> startArray();

	/**
	 * Commence un nouveau JsonObject � rajouter au JsonArray en cours de
	 * construction
	 * 
	 * @return un nouveau builder pour le nouveau JsonObject � construire et �
	 *         rajouter
	 */
	JsonObjectBuilder<T> startObject();

	/**
	 * Ajoute un {@link BigDecimal} comme valeur du JsonArray � construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(BigDecimal value);

	/**
	 * Ajoute un {@link Boolean} comme valeur du JsonArray � construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(Boolean value);

	/**
	 * Ajoute un {@link Double} comme valeur du JsonArray � construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(Double value);

	/**
	 * Ajoute un {@link Integer} comme valeur du JsonArray � construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(Integer value);

	/**
	 * Ajoute un {@link Long} comme valeur du JsonArray � construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(Long value);

	/**
	 * Ajoute un {@link String} comme valeur du JsonArray � construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(String value);

	/**
	 * Ajoute un {@link JsonObject} comme valeur du JsonArray � construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(JsonObject object);

	/**
	 * Ajoute un {@link JsonArray} comme valeur du JsonArray � construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(JsonArray array);

	/**
	 * Merge le JsonArray pass� en param�tre avec le JsonArray en cours de
	 * construction
	 * 
	 * <p>
	 * Toutes les valeurs du JsonArray � merger sont rajout�s comme valeurs du
	 * JsonArray en cours de construction
	 * </p>
	 * 
	 * @param object
	 *            le JsonObject � merger
	 * @return
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T merge(JsonArray array);
}