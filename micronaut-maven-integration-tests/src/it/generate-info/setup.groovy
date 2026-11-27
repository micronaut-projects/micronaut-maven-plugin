File readme = new File(basedir, 'README.md')
readme.text = 'generate info test\n'

def git(String... args) {
    def command = ['git'] + args.toList()
    def process = new ProcessBuilder(command)
            .directory(basedir as File)
            .redirectErrorStream(true)
            .start()
    def output = process.inputStream.text
    process.waitFor()
    assert process.exitValue() == 0: output
}

git('init')
git('config', 'user.email', 'dev@example.com')
git('config', 'user.name', 'Dev User')
git('remote', 'add', 'origin', 'https://example.com/private/repo.git')
git('add', 'README.md', 'pom.xml')
git('commit', '-m', 'Initial commit')
