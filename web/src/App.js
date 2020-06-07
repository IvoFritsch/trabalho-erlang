import React, { useEffect, useState } from 'react';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';
import './App.css';


function resolveStyleProduto(produto = '') {
  switch(produto.toLowerCase()){
    case "fanta": return { background: '#a63aae', color: 'white'}
    case "coca": return { background: '#fe001a', color: 'white'}
    default: return undefined
  }
}

function App() {
  const [estoque, setEstoque] = useState({QtdFanta: 0, QtdCoca: 0})
  const [produtores, setProdutores] = useState({})
  const [consumidores, setConsumidores] = useState({})
  
  useEffect(function conectaSocket(){
    let ws = new WebSocket('ws://localhost:8080')
    ws.onmessage = (m) => {
      if(!m.data.startsWith('{')) return
      const payload = JSON.parse(m.data)
      switch(payload.event){
        case "ESTOQUE":
          setEstoque(e => (
            {
              ...e, 
              ...payload
            }
          ))
          break
        case "PRODUTOR":
          setProdutores(p => (
            {
              ...p, 
              ...{ [payload.np]: payload.produto }
            }
          ))
          break
        case "CONSUMIDOR-CONSUMINDO":
            setConsumidores(c => (
              {
                ...c, 
                ...{ [payload.nc]: { acao: 'Consumindo', produto:payload.produto} }
              }
            ))
            break
        case "CONSUMIDOR-ESPERANDO":
              setConsumidores(c => (
                {
                  ...c, 
                  ...{ [payload.nc]: { acao: 'Esperando', produto:payload.produto} }
                }
              ))
              break
        default: break
      }
    }
  }, [])

  return (
    <Grid container spacing={2} style={{paddingTop: '20px'}}>
      
      <Grid item style={{flexGrow: '1'}}>
          <Grid container justify="center" spacing={8}>

          <Grid item>
            {Object.keys(produtores).map(np => 
                <React.Fragment key={np}>
                  <Grid item xs={12}>
                    <Paper style={resolveStyleProduto(produtores[np])}>
                      <div style={{padding: '10px', textAlign: 'center', fontWeight: 'bold'}}>
                        <h2>Produtor {np}</h2>
                        <hr/>
                        {produtores[np] ? 
                        `Produzindo: ${produtores[np]}` :
                        'Preparando...'}
                      </div>
                    </Paper>
                  </Grid>
                  <br/>
                </React.Fragment>
            )}
          </Grid>
          <Grid item>
            <Paper>
              <div style={{padding: '10px', textAlign: 'center', fontWeight: 'bold'}}>
                <h2>Estoque</h2>
                <hr/>
                Qtd. Cocas: {estoque.QtdCoca}
                <br/>
                Qtd. Fantas: {estoque.QtdFanta}
              </div>
            </Paper>
          </Grid>
          <Grid item>
            {Object.keys(consumidores).map(nc => 
                <React.Fragment key={nc}>
                  <Grid item xs={12}>
                    <Paper style={resolveStyleProduto(consumidores[nc].produto)}>
                      <div style={{padding: '10px', textAlign: 'center', fontWeight: 'bold'}}>
                        <h2>Consumidor {nc}</h2>
                        <hr/>
                        {consumidores[nc].acao}: {consumidores[nc].produto}
                      </div>
                    </Paper>
                  </Grid>
                  <br/>
                </React.Fragment>
            )}
          </Grid>
      </Grid>
      </Grid>
    </Grid>
  );
}

export default App;
