-- 1. Obt�n un listado de todos los circuitos que se encuentren en Espa�a.
SELECT c.Ref FROM circuit c WHERE country = 'Spain';

-- 2. Obt�n todos los pilotos que formaron parte de la escuder�a Ferrari en la temporada 2009
-- y ord�nalos alfab�ticamente de manera inversa (los nombres que empiezan por la letra
-- Z deber�n aparecer en primer lugar).

SELECT d.Ref, d.name, d.surname FROM driver d
JOIN driver_drives_for_constructor dc ON d.Id = dc.driver_id
JOIN constructor c ON dc.constructor_id = c.Id
WHERE dc.season = 2009 AND c.name = 'Ferrari'
ORDER BY d.name DESC;

--3. �Cu�l es el nombre de la �ltima carrera celebrada?
SELECT TOP 1 r.name, r.date FROM race r
ORDER BY r.date DESC;

--4. Obt�n la posici�n media del piloto Lewis Hamilton para todas las carreras en las que
--particip�.
SELECT AVG(CAST(r.positionOrder AS FLOAT)) posicion_media FROM result r
JOIN driver d ON r.driver_id = d.Id
WHERE d.name = 'Lewis' AND d.surname = 'Hamilton';

--5. �Qu� piloto ha participado en el mayor n�mero de carreras?
SELECT TOP 1 d.Ref, d.name, d.surname, COUNT(*) n_carreras FROM result r
JOIN driver d ON d.Id = r.driver_id
GROUP BY d.Ref, d.name, d.surname
ORDER BY COUNT(*) DESC;

--6. �Qu� pilotos han participado en al menos 20 temporadas?
SELECT d.Ref, d.name, d.surname, COUNT(DISTINCT dc.season) n_temporadas FROM driver d
JOIN driver_drives_for_constructor dc ON d.Id = dc.driver_id
GROUP BY d.Ref, d.name, d.surname
-- HAVING COUNT(DISTINCT season) >= 20 condici�n comentada para ver si exist�an resultados
ORDER BY n_temporadas DESC;

--7. Obt�n un listado de los pilotos cuyo nombre empiece por �Joe� y ordena sus apellidos
--de forma descendente.
SELECT d.Ref, d.name, d.surname FROM driver d
WHERE d.name LIKE 'Joe%'
ORDER BY d.surname DESC;

--8. Calcula los puntos totales que obtuvo el piloto Fernando Alonso en la temporada 2005.
SELECT SUM(rs.points) puntos FROM driver d
JOIN result rs ON d.Id = rs.driver_id
JOIN race ra ON rs.race_id = ra.Id
WHERE d.name = 'Fernando' AND d.surname = 'Alonso' AND ra.season = 2005;

--9. Obt�n un listado de todos los pilotos que han formado parte de la escuder�a Renault.
SELECT DISTINCT d.Ref, d.name, d.surname FROM driver d
JOIN driver_drives_for_constructor dc ON d.Id = dc.driver_id
JOIN constructor c ON dc.constructor_id = c.Id
WHERE c.name = 'Renault'

--10. Calcula la diferencia de puntos obtenidos en la temporada 2010 entre los pilotos
--Fernando Alonso y Lewis Hamilton.

-- (Aunque no hemos aprendido la expresi�n CASE en clase, estaba familiarizado con ella y utilizarla me parec�a la opci�n m�s sencilla)
SELECT 
	SUM(CASE WHEN d.name = 'Fernando' AND d.surname = 'Alonso' AND ra.season = 2010 THEN rs.points END) puntos_alonso,
	SUM(CASE WHEN d.name = 'Lewis' AND d.surname = 'Hamilton' AND ra.season = 2010 THEN rs.points END) puntos_hamilton,
	SUM(CASE WHEN d.name = 'Fernando' AND d.surname = 'Alonso' AND ra.season = 2010 THEN rs.points END)
		- SUM(CASE WHEN d.name = 'Lewis' AND d.surname = 'Hamilton' AND ra.season = 2010 THEN rs.points END) [diferencia puntos]
FROM driver d
JOIN result rs ON d.Id = rs.driver_id
JOIN race ra ON rs.race_id = ra.Id;