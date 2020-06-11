import React, { useEffect, useState } from 'react';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';
import './App.css';


function resolveStyleProduto(produto = '', acao = '') {
  const styleAcao = {}
  if(acao.toLowerCase() === 'esperando'){
    styleAcao.background = undefined
    styleAcao.color = undefined
  }

  switch(produto.toLowerCase()){
    case "fanta": return { background: '#a63aae', color: 'white', border: '6px solid #a63aae', ...styleAcao}
    case "coca": return { background: '#fe001a', color: 'white', border: '6px solid #fe001a', ...styleAcao}
    default: return { border: '6px solid white'}
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
              [payload.np]: payload.produto
            }
          ))
          break
        case "CONSUMIDOR-CONSUMINDO":
            setConsumidores(c => (
              {
                ...c, 
                [payload.nc]: { acao: 'Consumindo', produto:payload.produto}
              }
            ))
            break
        case "CONSUMIDOR-ESPERANDO":
              setConsumidores(c => (
                {
                  ...c, 
                  [payload.nc]: { acao: 'Esperando', produto:payload.produto}
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
                        <h2 style={{width: '170px'}}>Produtor {np}</h2>
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
            <Paper style={{border: '6px solid black'}}>
              <div style={{padding: '10px', textAlign: 'center', fontWeight: 'bold'}}>
                <h2 style={{width: '170px'}}>Estoque</h2>
                <hr/>
                <span style={{background: '#fe001a', color: 'white', padding: '3px'}}>Qtd. Cocas: {estoque.QtdCoca}</span>
                <br/>
                <br/>
                <span style={{background: '#a63aae', color: 'white', padding: '3px'}}>Qtd. Fantas: {estoque.QtdFanta}</span>
              </div>
            </Paper>
          </Grid>
          <Grid item>
            {Object.keys(consumidores).map(nc => 
                <React.Fragment key={nc}>
                  <Grid item xs={12}>
                    <Paper style={resolveStyleProduto(consumidores[nc].produto, consumidores[nc].acao)}>
                      <div style={{padding: '10px', textAlign: 'center', fontWeight: 'bold'}}>
                        <h2 style={{width: '170px'}}>Consumidor {nc}</h2>
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
