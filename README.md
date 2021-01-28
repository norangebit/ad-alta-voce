# Ad Alta Voce

Script per la generazione dei Feed Podcast di [Ad Alta Voce](https://www.raiplayradio.it/programmi/adaltavoce/).

## Utilizzo

### Installazione

Una volta scaricato il repository è possibile installare il programma tramite `stack` attraverso il seguente comando:

```bash
stack install
```

L'eseguibile può essere chiamato attraverso il comando `loud`.

### Generazione di un singolo podcast

È possibile generare il feed podcast di un audiobook tramite il seguente comando:

```bash
loud single <audiobook-url>
```

Una lista di tutti gli audiolibri di Ad Alta Voce può essere recuperata al seguente [link](https://www.raiplayradio.it/programmi/adaltavoce/archivio/audiolibri/tutte/).

### Generazione di tutti i podcast

È possibile generare in automatico tutti i feed podcast degli audiobook attraverso il seguente comando:

```bash
loud all
```

### Generazione indice

Attraverso il comando `all` è possibile generare anche un indice di tutti gli audiolibri generati attraverso il comando:

```bash
loud all --index
```

Inoltre attraverso il flag `--index-template` è possibile specificare il template [mustache](https://mustache.github.io/mustache.5.html) dell'indice.
Il file del template deve trovarsi nella working directory ed avere un del tipo `<nome>.<estenzione>.mustache`.

I parametri del template sono:

- `entries`: lista dei feed podcast degli audiolibri
- `audiobook-file`: nome del file xml
- `audiobook-author`: autore dell'audiolibro
- `audiobook-title`: titolo dell'audiolibro

Di seguito è riportato un esempio di template html per l'indice.

```html
<html>
	<head>
		<title>Ad alta voce - Podcast non ufficiale</title>
		<meta charset="utf-8" />
	</head>
  <h1>Lista audiobook</h1>
  <ul>
  {{#entries}}
    <li><a href='{{audiobook-file}}'>{{audiobook-title}}</a> {{audiobook-author}}</li>
  {{/entries}}
  </ul>
</html>	
```

### Help page

È possibile visualizzare le opzioni aggiuntive attraverso la *help page* richiamabile attraverso il seguente comando:

```bash
loud --help
```

## Utilizzo tramite stack

Alternativamente è possibile utilizzare `loud` attraverso `stack` senza doverlo installare.
In questo caso i comandi sono:

```bash
stack run -- single <audiobook-url>
stack run -- all
```

