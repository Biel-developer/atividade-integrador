(ns integrador.core)

(defn cadastrar-alunos []
    (println "\n=== Cadastra alunos")
    (loop [alunos []]
        (print "Nome do aluno ou pressione Enter para Sair: ")
        (flush)
        (let [nome (read-line)]
            (if (empty? nome)
                alunos
                (do
                   (print "Nota do aluno: ")
                   (flush)
                   (let [nota (Double/parseDouble (read-line))
                        novo-aluno {:nome nome :nota nota}]
                      (recur (conj alunos novo-aluno))))))))

(defn calcular-status [aluno]
  (assoc aluno :status (if (>= (:nota aluno) 6.0) "Aprovado" "Reprovado")))

(defn relatorio-notas [alunos]
  (println "\n=== Relatorio de Notas ===")
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (let [alunos-status (map calcular-status alunos)
          aprovados (filter #(= (:status %) "Aprovado") alunos-status)
          media (/ (reduce + (map :nota alunos-status)) (count alunos-status))]
      (println "\nAlunos aprovados:")
      (doseq [a aprovados]
        (println (:nome a) "- Nota:" (:nota a) "- Status:" (:status a)))
      (println "\nMedia geral da turma:" (format "%.2f" media)))))

(defn estatisticas-gerais [alunos]
  (println "\n=== Estatisticas Gerais ===")
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (let [alunos-status (map calcular-status alunos)
          total (count alunos-status)
          aprovados (count (filter #(= (:status %) "Aprovado") alunos-status))
          reprovados (- total aprovados)
          notas (map :nota alunos-status)
          maior (apply max notas)
          menor (apply min notas)
          media (/ (reduce + notas) total)]
      (println "Total de alunos:" total)
      (println "Aprovados:" aprovados)
      (println "Reprovados:" reprovados)
      (println "Maior nota:" maior)
      (println "Menor nota:" menor)
      (println "Media geral:" (format "%.2f" media)))))

(defn buscar-aluno [alunos]
  (println "\n=== Buscar Aluno ===")
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (do
      (print "Digite o nome do aluno: ")
      (flush)
      (let [nome (read-line)
            alunos-status (map calcular-status alunos)
            encontrado (filter #(= (clojure.string/lower-case (:nome %))
                                   (clojure.string/lower-case nome))
                               alunos-status)]
        (if (empty? encontrado)
          (println "Aluno nao encontrado.")
          (doseq [a encontrado]
            (println "Nome:" (:nome a)
                     "| Nota:" (:nota a)
                     "| Status:" (:status a))))))))

(defn -main []
  (loop [alunos []]
    (println "\n=== MENU PRINCIPAL ===")
    (println "1 - Cadastrar Alunos")
    (println "2 - Relatorio de Notas")
    (println "3 - Estatisticas Gerais")
    (println "4 - Buscar Aluno (extra)")
    (println "0 - Sair")
    (print "Escolha uma opcao: ")
    (flush)
    (let [opcao (read-line)]
      (cond
        (= opcao "1") (recur (cadastrar-alunos))
        (= opcao "2") (do (relatorio-notas alunos) (recur alunos))
        (= opcao "3") (do (estatisticas-gerais alunos) (recur alunos))
        (= opcao "4") (do (buscar-aluno alunos) (recur alunos))
        (= opcao "0") (println "Saindo do sistema...")
        :else (do (println "Opcao invalida!") (recur alunos))))))